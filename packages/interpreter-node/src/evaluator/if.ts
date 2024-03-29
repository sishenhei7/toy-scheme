import { type SchemeData, SchemeSym, SchemeList, SchemeCont, SchemeBoolean } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (if predicate then_value else_value)
 */
export default class IfEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'if'
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    return this.evaluator.evaluate(
      node.cadr(),
      env,
      new SchemeCont((data: SchemeData) =>
        SchemeBoolean.isTrue(data)
          ? this.evaluator.evaluate(node.caddr(), env, cont)
          : this.evaluator.evaluate(node.cadddr(), env, cont)
      )
    )
  }
}
