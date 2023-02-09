import type { BaseData } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法：
 * (cond
 *     (predicate_1 clauses_1)
 *     (predicate_1 clauses_1)
 *     (else        clauses_else))
 */

export class CondEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(): boolean {
    return false
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return node
  }
}
