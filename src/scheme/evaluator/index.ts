import { type SchemeData, Continuation, SchemeList, SchemeSym } from '../parser/data'
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

export interface IEvaluator {
  matches(value: string, env?: Env): boolean
  evaluate(node: SchemeData, env: Env, cont: Continuation): SchemeData
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
      new ProcEvaluator(this)
    ]
  }

  public evaluate(node: SchemeData, env: Env, cont: Continuation = Continuation.Identity): SchemeData {
    // is a sentence
    if (SchemeList.matches(node) && node.shouldEval) {
      const peek = node.car()

      if (SchemeSym.matches(peek)) {
        for (const evaluator of this.evaluators) {
          if (evaluator.matches(peek.value, env)) {
            return evaluator.evaluate(node, env, cont)
          }
        }
      }

      return this.evaluateList(node, env, cont)
    }

    if (SchemeSym.matches(node)) {
      return cont.call(env.get(node.value))
    }

    return cont.call(node)
  }

  public evaluateList(node: SchemeList, env: Env, cont: Continuation = Continuation.Identity): SchemeData {
    let res = this.evaluate(node.car(), env, cont)
    if (!SchemeList.isNil(node.cdr())) {
      res = this.evaluateList(SchemeList.cast(node.cdr()), env, cont)
    }
    return res
  }
}
