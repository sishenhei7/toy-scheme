import { SchemeBoolean, SchemeSym, type NodeData, SchemeExp, type Continuation, type SchemeData } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

/**
 * 语法：
 * (cond
 *     (predicate_1 clauses_1)
 *     (predicate_2 clauses_2)
 *     (else        clauses_else))
 */

export default class CondEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'cond'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    let currentNode = node.next

    while (currentNode) {
      const predict = this.getPredicate(currentNode)

      // 匹配 else 语句
      if (SchemeSym.matches(predict) && predict.tag === 'else') {
        return this.evaluator.evaluate(this.getValue(currentNode), env, cont)
      }

      // 匹配第一个成功的 predict
      const predictValue = this.evaluator.evaluate(predict, env, cont)
      if (SchemeBoolean.isTrue(predictValue)) {
        return this.evaluator.evaluate(this.getValue(currentNode), env, cont)
      }

      currentNode = currentNode.next
    }

    assert(true, 'Error: cond clause evaluate error!')
    return null as never
  }

  private getPredicate(node: NodeData): NodeData {
    assert(SchemeExp.matches(node), 'Syntax error: cond clause should have brackets!')
    assert(node?.body, 'Syntax error: cond clause need predicate clause!')
    return node.body
  }

  private getValue(node: NodeData): NodeData {
    assert(SchemeExp.matches(node), 'Syntax error: cond clause should have brackets!')
    assert(node?.body?.next, 'Syntax error: cond clause need value clause!')
    return node.body.next
  }
}
