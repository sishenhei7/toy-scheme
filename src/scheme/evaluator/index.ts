import { type Thunk, type SchemeData, SchemeCont, SchemeList, SchemeSym, SchemeProc } from '../parser/data'
import type { Env } from '../env'
import BuildInEvaluator from './buildin'
import LetEvaluator from './let'
import LambdaEvaluator from './lambda'
import IfEvaluator from './if'
import DefineEvaluator from './define'
import CondEvaluator from './cond'
import CallCCEvaluator from './call-cc'
import SetEvaluator from './set'
import BeginEvaluator from './begin'
import ProcEvaluator from './proc'
import { assert } from '../utils'

export interface IEvaluator {
  matches(node: SchemeData, env?: Env): boolean
  evaluate(node: SchemeData, env: Env, cont: SchemeCont): Thunk
}

export class Evaluator {
  private evaluators: IEvaluator[]
  private stepCount = 0
  private step = 0

  constructor() {
    this.evaluators = [
      new BuildInEvaluator(this),
      new LetEvaluator(this),
      new LambdaEvaluator(this),
      new IfEvaluator(this),
      new DefineEvaluator(this),
      new CondEvaluator(this),
      new CallCCEvaluator(this),
      new SetEvaluator(this),
      new BeginEvaluator(this),
      new ProcEvaluator(this) // should be the last one
    ]
  }

  public trampoline(node: Thunk | SchemeData) {
    while (typeof node === 'function') {
      // console.log(111111, node)
      node = node()
    }
    return node
  }

  public evaluate(node: SchemeData, env: Env, cont: SchemeCont = SchemeCont.Identity): Thunk {
    if (this.step ++ > 500) {
      debugger
    }
    if (SchemeList.matches(node) && node.shouldEval) {
      const peek = node.car()

      // 当节点是 SchemeCont 的时候，丢弃当前的 cont，直接执行 SchemeCont
      if (SchemeCont.matches(peek) && !SchemeList.isNil(node.cdr())) {
        return () => this.evaluate(node.cadr(), env, peek)
      }

      for (const evaluator of this.evaluators) {
        if (evaluator.matches(peek, env)) {
          return () => evaluator.evaluate(node, env, cont)
        }
      }

      assert(false, `evaluate error: ${node.toString}`)
    }

    if (SchemeSym.matches(node)) {
      return cont.call(env.get(node.value))
    }

    return cont.call(node)
  }

  // 这里很重要，下一个语句是通过上一个语句的cont进行执行的！
  public evaluateList(node: SchemeList, env: Env, cont: SchemeCont = SchemeCont.Identity): Thunk {
    return this.evaluate(node.car(), env, new SchemeCont((data: SchemeData) => {
      if (SchemeList.isNil(node.cdr())) {
        return cont.call(data)
      }
      return this.evaluateList(node.cdr(), env, cont)
    }))
  }

  // TODO: 缺少一个 application 的语法
  public evaluateApplication(node: SchemeList, env: Env, cont: SchemeCont = SchemeCont.Identity) {

  }
}
