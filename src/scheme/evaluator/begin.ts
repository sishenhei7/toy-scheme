import type { SchemeData, SchemeCont, SchemeList, Cont } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (begin
 *    (set! amount m)
 *    (set! amount n)
 *    amout)
 */
export default class BeginEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(value: string): boolean {
    return value === 'begin'
  }

  public evaluate(node: SchemeList, env: Env, cont: Cont): SchemeData {
    return this.evaluator.evaluateList(node.cdr(), env, cont)
  }
}
