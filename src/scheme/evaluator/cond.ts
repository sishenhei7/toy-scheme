import { SchemeBoolean, SchemeSym, type BaseData, type SchemeExp } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'
import { assert } from '../utils'

/**
 * 语法：
 * (cond
 *     (predicate_1 clauses_1)
 *     (predicate_2 clauses_2)
 *     (else        clauses_else))
 */

export class CondEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'cond'
  }

  public evaluate(node: SchemeExp, env: Env, cont: Cont): BaseData {
    let currentNode = node.body
    let res = null

    while (currentNode) {
      const predict = this.getPredicate(currentNode)

      if (SchemeSym.matches(predict) && predict.tag === 'else') {
        res = this.evaluator.evaluate(this.getValue(currentNode), env, cont)
        break
      }

      const predictValue = this.evaluator.evaluate(predict, env, cont)
      if (SchemeBoolean.isTrue(predictValue)) {
        res = this.evaluator.evaluate(this.getValue(currentNode), env, cont)
        break
      }

      currentNode = currentNode.next
    }

    assert(res, 'Syntax error: cond clause evaluate error!')
    return res
  }

  private getPredicate(node: BaseData | null): BaseData {
    assert(node, 'Syntax error: cond clause need predicate clause!')
    return node
  }

  private getValue(node: BaseData | null): BaseData {
    assert(node?.next, 'Syntax error: cond clause need value clause!')
    return node.next
  }
}
