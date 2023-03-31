import { SchemeBoolean, SchemeCont, SchemeList, SchemeSym, type SchemeData } from '../parser/data'
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

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    return this.evaluateConditions(node.cdr(), env, cont)
  }

  private evaluateConditions(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    if (!SchemeList.isNil(node)) {
      const currentNode = SchemeList.cast(node.car())
      const predictNode = currentNode.car()

      // 匹配 else 语句
      if (SchemeSym.matches(predictNode) && predictNode.value === 'else') {
        return this.evaluator.evaluate(currentNode.cdr(), env, cont)
      }

      // 匹配第一个成功的 predict
      return this.evaluator.evaluate(predictNode, env, new SchemeCont((data: SchemeData) => {
        if (SchemeBoolean.isTrue(data)) {
          return this.evaluator.evaluate(currentNode.cdr(), env, cont)
        }
        return this.evaluateConditions(node.cdr(), env, cont)
      }))
    }

    assert(false, 'Cond error: no predict is matched!')
  }
}
