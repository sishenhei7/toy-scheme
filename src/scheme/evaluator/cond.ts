import { SchemeBoolean, SchemeList, SchemeSym, type Continuation, type SchemeData } from '../parser/data'
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

  public matches(value: string): boolean {
    return value === 'cond'
  }

  public evaluate(node: SchemeList, env: Env, cont: Continuation): SchemeData {
    let currentNode: SchemeList
    while (!SchemeList.isNil(currentNode = node.cdr())) {
      const predictNode = currentNode.car()

      // 匹配 else 语句
      if (SchemeSym.cast(predictNode).value === 'else') {
        return this.evaluator.evaluate(currentNode.cdr(), env, cont)
      }

      // 匹配第一个成功的 predict
      const predictValue = this.evaluator.evaluate(predictNode, env)
      if (SchemeBoolean.isTrue(predictValue)) {
        return this.evaluator.evaluate(currentNode.cdr(), env, cont)
      }
    }

    assert(false, 'Error: at least one cond clause is required!')
  }
}
