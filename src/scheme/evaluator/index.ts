import { type NodeData, type Cont, type SchemeData, SchemeExp, SchemeSym } from '../parser/data'
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
  matches(tag: string, env?: Env): boolean
  evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData
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

  // TODO: 添加一个 evaluateList
  public evaluate(node: NodeData | null, env: Env, cont: Cont): SchemeData {
    // 暂不支持()这样空语句的形式
    assert(node, `Evaluating error: unexpected ${node}`)

    // 去掉 expression 两边的括号
    while (SchemeExp.matches(node)) {
      const value = this.evaluate(node.body, env, cont)

      if (!node.next) {
        return value
      }

      node = node.next
    }

    // expression的第一个token
    if (SchemeSym.matches(node)) {
      for (const evaluator of this.evaluators) {
        if (evaluator.matches(node.tag, env)) {
          return evaluator.evaluate(node, env, cont)
        }
      }
    }

    // TODO: refine
    return node as SchemeData
  }
}
