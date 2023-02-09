import type { BaseData } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法：
 * (set! var (* var 10))
 */
export class SetEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(): boolean {
    return false
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return node
  }
}
