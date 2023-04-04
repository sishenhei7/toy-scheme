import { type SchemeData, type SchemeCont, type SchemeList, type Thunk, SchemeSym } from '../parser/data'
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

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'begin'
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluator.evaluateList(node.cdr(), env, cont)
  }
}
