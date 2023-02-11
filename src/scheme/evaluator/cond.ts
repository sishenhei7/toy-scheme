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

    while (currentNode) {
      const predict = this.getPredicate(currentNode)

      if (SchemeSym.matches(predict) && predict.tag === 'else') {
        return this.evaluator.evaluate(this.getValue(currentNode), env, cont)
      }

      const predictValue = this.evaluator.evaluate(predict, env, cont)
      if (SchemeBoolean.isTrue(predictValue)) {
        return this.evaluator.evaluate(this.getValue(currentNode), env, cont)
      }

      currentNode = currentNode.next
    }

    assert(true, 'Error: cond clause evaluate error!')
    return null as never
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
