import { type SchemeData, SchemeCont, SchemeList, SchemeSym } from '../parser/data'
import { Env } from '../env'
import BuildInEvaluator from './buildin'
import LetEvaluator from './let'
import LambdaEvaluator from './lambda'
import IfEvaluator from './if'
import DefineEvaluator from './define'
import CondEvaluator from './cond'
import CallCCEvaluator from './call-cc'
import SetEvaluator from './set'
import BeginEvaluator from './begin'
import ContEvaluator from './cont'
import ProcEvaluator from './proc'
import { assert } from '../utils'

export interface IEvaluator {
  matches(node: SchemeData): boolean
  evaluate(node: SchemeData, env: Env, cont: SchemeCont): SchemeData
}

export interface EvaluatorOptions {
  log?: Function
  prompt?: Function
}

export class Evaluator {
  private evaluators: IEvaluator[]
  private stepCount = 0
  private step = 0

  constructor(options?: EvaluatorOptions) {
    this.evaluators = [
      new BuildInEvaluator(this, options),
      new LetEvaluator(this),
      new LambdaEvaluator(this),
      new IfEvaluator(this),
      new DefineEvaluator(this),
      new CondEvaluator(this),
      new CallCCEvaluator(this),
      new SetEvaluator(this),
      new BeginEvaluator(this),
      new ContEvaluator(this),
      new ProcEvaluator(this) // should be the last one
    ]
  }

  public run(node: SchemeList): SchemeData {
    const res = this.evaluateList(node, new Env())
    return this.trampoline(res)
  }

  public trampoline(node: SchemeData): SchemeData {
    while (SchemeCont.matches(node)) {
      // console.log(111111, node)
      node = node.call()
    }
    return node
  }

  public evaluate(node: SchemeData, env: Env, cont: SchemeCont): SchemeData {
    if (SchemeList.matches(node) && !SchemeList.isNil(node) && node.shouldEval) {
      const peek = node.car()

      for (const evaluator of this.evaluators) {
        if (evaluator.matches(peek)) {
          return new SchemeCont(() => evaluator.evaluate(node, env, cont))
            .setEnv(env)
            .setLocationInfo(node.range)
        }
      }

      assert(false, `evaluate error: ${node.toString}`)
    }

    if (SchemeSym.matches(node)) {
      return cont
        .setValue(env.get(node.value))
        .setEnv(env)
        .setLocationInfo(node.range)
    }

    return cont
      .setValue(node)
      .setEnv(env)
      .setLocationInfo(node.range)
  }

  // 这里很重要，下一个语句是通过上一个语句的cont进行执行的！
  public evaluateList(node: SchemeList, env: Env, cont: SchemeCont = SchemeCont.Identity): SchemeData {
    return this.evaluate(node.car(), env, new SchemeCont((data: SchemeData) => {
      if (SchemeList.isNil(node.cdr())) {
        return cont.setValue(data)
      }
      return this.evaluateList(node.cdr(), env, cont)
    }))
  }
}
