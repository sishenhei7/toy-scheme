import { type SchemeData, type SchemeCont, SchemeList, SchemeProc, SchemeSym } from '../parser/data'
import { Env, StackFrame } from '../env'
import type { IEvaluator, Evaluator } from './index'
import ProcEvaluator from './proc'
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

  public matches(value: string): boolean {
    return value === 'call-with-current-continuation'
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    const escapedProc = this.buildEscapedProc(env, cont)
    const paramProc = this.evaluator.evaluate(node.cadr(), env)
    assert(SchemeProc.matches(paramProc), 'callcc args evaluate eror!')

    const parentStackframe = env.getStackFrame()
    const newEnv = new Env(env, new StackFrame(parentStackframe))
    const virtualName = 'callcc'
    const virtualNode = SchemeList.buildFromArray([new SchemeSym(virtualName), escapedProc])
    newEnv.define(virtualName, paramProc)
    return this.evaluator.evaluate(virtualNode, newEnv, cont)
  }

  private buildEscapedProc(env: Env, cont: SchemeCont): SchemeProc {
    const paramName = '(escaped-proc-param)'
    const params = SchemeList.buildFromAtom(new SchemeSym(paramName))
    const body = SchemeList.buildFromArray([cont, new SchemeSym(paramName)])
    return new SchemeProc('<<captured contiuation>>', params, body, env)
  }


}
