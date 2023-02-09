import { type BaseData, SchemeExp, SchemeSym, SchemeNil } from '../parser/data'
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
  public evaluate(node: BaseData | null, env: Env, cont: Cont): BaseData {
    if (!node) {
      return new SchemeNil()
    }

    if (SchemeExp.matches(node)) {
      let currentNode: SchemeExp | null = node
      let res = new SchemeNil()
      while (currentNode) {
        res = this.evaluate(currentNode.body, env, cont)
        currentNode = currentNode.next
      }
      return res
    }

    if (SchemeSym.matches(node) && node.body) {
      for (const evaluator of this.evaluators) {
        if (evaluator.matches(node)) {
          return evaluator.evaluate(node, env, cont)
        }
      }
    }

    return node
  }
}
