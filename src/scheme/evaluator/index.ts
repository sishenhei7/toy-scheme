import { type SchemeData, Continuation, SchemeList, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import { assert } from '../utils'
import BuildInEvaluator from './buildin'
import IfEvaluator from './if'
import DefineEvaluator from './define'
import CondEvaluator from './cond'
import CallCCEvaluator from './call-cc'
import SetEvaluator from './set'
import BeginEvaluator from './begin'
import ProcEvaluator from './proc'
import VariableEvaluator from './variable'

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
      new IfEvaluator(this),
      new DefineEvaluator(this),
      new CondEvaluator(this),
      new CallCCEvaluator(this),
      new SetEvaluator(this),
      new BeginEvaluator(this),
      new ProcEvaluator(this),
      new VariableEvaluator()
    ]
  }

  public evaluate(node: SchemeData, env: Env, cont: Continuation = Continuation.Identity): SchemeData {
    assert(node, `Evaluating error: unexpected ${node}`)

    // is a sentence
    if (SchemeList.matches(node) && node.shouldEval) {
      const peek = node.car()

      // evaluator
      if (SchemeSym.matches(peek)) {
        for (const evaluator of this.evaluators) {
          if (evaluator.matches(peek.value, env)) {
            return evaluator.evaluate(node, env, cont)
          }
        }
      }

      // list
      return this.evaluateList(node, env, cont)
    }

    // is a SchemeData
    return node as SchemeData
  }

  public evaluateList(node: SchemeList, env: Env, cont: Continuation = Continuation.Identity): SchemeData {
    let res = this.evaluate(node.car(), env, cont)
    if (!SchemeList.isNil(node.cdr())) {
      res = this.evaluateList(SchemeList.cast(node.cdr()), env, cont)
    }
    return res
  }
}
