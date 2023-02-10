import { type BaseData, SchemeExp, SchemeSym, SchemeNil } from '../parser/data'
import type { Env } from '../env'
import { IfEvaluator } from './if'
import { DefineEvaluator } from './define'
import { CondEvaluator } from './cond'
import { CallCCEvaluator } from './call-cc'
import { SetEvaluator } from './set'
import { BeginEvaluator } from './begin'

export type Cont = (node: BaseData) => BaseData

export interface IEvaluator {
  matches(tag: string): boolean
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
      new BeginEvaluator(this)
    ]
  }

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
