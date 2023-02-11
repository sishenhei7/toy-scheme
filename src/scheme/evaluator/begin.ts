import type { BaseData } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法：
 * (begin
 *    (set! amount m)
 *    (set! amount n)
 *    amout)
 */
export class BeginEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'begin'
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return node
  }

  private getCont(node: BaseData, env: Env, cont: Cont) {
    return (val: BaseData) => {

    }
  }
}
