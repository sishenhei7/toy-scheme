import type { BaseData } from '../parser/data'
import type { Env } from '../env'
import { IfEvaluator } from './if'
import { DefineEvaluator } from './define'
import { CondEvaluator } from './cond'
import { CallCCEvaluator } from './call-cc'
import { SetEvaluator } from './set'
import { BeginEvaluator } from './begin'
import { QuoteEvaluator } from './quote'

export type Cont = (node: BaseData) => BaseData

export interface IEvaluator {
  matches(node: BaseData): boolean
  evaluate(node: BaseData, env: Env, cont: Cont): BaseData
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
      new QuoteEvaluator(this)
    ]
  }

  // TODO: add return type
  public evaluate(node: BaseData | null, env: Env, cont: Cont): any {
    if (!node) {
      return null
    }

    // if (node instanceof NodeContainer) {
    //   return this.evaluate(node.body, env, cont)
    // }

    // // TODO: 处理 EOF
    // switch (node.type) {
    //   case TokenType.Number:
    //     return Number(node.value)
    //   case TokenType.String:
    //     return String(node.value)
    //   case TokenType.Boolean:
    //     return Boolean(node.value)
    //   case TokenType.Quote || TokenType.Symbol:
    //     for (const evaluator of this.evaluators) {
    //       if (evaluator.matches(node)) {
    //         return evaluator.evaluate(node, env, cont)
    //       }
    //     }
    //     throw Error(`Evaluate Error: unknown quote or symbol: ${node.type}!`)
    //   default:
    //     throw Error(`Evaluate Error: unknown token type: ${node.type}!`)
    // }
  }
}
