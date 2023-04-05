import { type Thunk, type SchemeData, SchemeCont, SchemeList, SchemeProc, SchemeSym } from '../parser/data'
import { Env, StackFrame } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

/**
 * 语法（后面加一个 lambda 语句）：
 * (call-with-current-continuation
 *    (lambda (resume-here)
 *      (set! control-state resume-here)
 *      (return element)))
 */
export default class CallCCEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'call-with-current-continuation'
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluator.evaluate(node.cadr(), env, new SchemeCont((proc: SchemeData) => {
      assert(SchemeProc.matches(proc), 'callcc args evaluate eror!')
      const newEnv = new Env(env, new StackFrame(env.getStackFrame()))
      const virtualName = 'callcc'
      const virtualNode = SchemeList.buildFromArray([
        new SchemeSym(virtualName),
        this.buildEscapedProc(env, cont)
      ])
      newEnv.define(virtualName, proc)
      return this.evaluator.evaluate(virtualNode, newEnv, cont)
    }))
  }

  private buildEscapedProc(env: Env, cont: SchemeCont): SchemeProc {
    const paramName = '(escaped-proc-param)'
    const params = SchemeList.buildFromAtom(new SchemeSym(paramName))
    const virtualNode = SchemeList.buildFromArray([cont as any, new SchemeSym(paramName)])
    const body = SchemeList.buildFromAtom(virtualNode) // need to wrap by a list
    return new SchemeProc('<<captured contiuation>>', params, body, env)
  }
}
