import { type BaseData, type Cont, type SchemeData, SchemeExp, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import { assert } from '../utils'
import { IfEvaluator } from './if'
import { DefineEvaluator } from './define'
import { CondEvaluator } from './cond'
import { CallCCEvaluator } from './call-cc'
import { SetEvaluator } from './set'
import { BeginEvaluator } from './begin'
import { VariableEvaluator } from './variable'
import { FunctionEvaluator } from './function'

export interface IEvaluator {
  matches(node: SchemeData): boolean
  evaluate(node: BaseData, env: Env, cont: Cont): SchemeData
}

export class Evaluator {
  private evaluators: IEvaluator[]
  private stepCount = 0
  private step = 0

  constructor() {
    this.evaluators = [
      new IfEvaluator(this),
      new DefineEvaluator(this),
      new CondEvaluator(this),
      new CallCCEvaluator(this),
      new SetEvaluator(this),
      new BeginEvaluator(this),
      new FunctionEvaluator(this),
      // 需要放在最下面
      new VariableEvaluator()
    ]
  }

  public evaluate(node: BaseData | null, env: Env, cont: Cont): SchemeData {
    // 暂不支持()这样空语句的形式
    assert(node, `Evaluating error: unexpected ${node}`)

    // 去掉 expression 两边的括号
    if (SchemeExp.matches(node)) {
      return this.evaluate(node.body, env, cont)
    }

    // expression的第一个token
    if (SchemeSym.matches(node)) {
      for (const evaluator of this.evaluators) {
        if (evaluator.matches(node.tag)) {
          return evaluator.evaluate(node, env, cont)
        }
      }
    }

    return node
  }
}
