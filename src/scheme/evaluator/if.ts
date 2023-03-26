import { type SchemeData, SchemeList, Continuation, SchemeBoolean } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (if predicate then_value else_value)
 */
export default class IfEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'if'
  }

  public evaluate(node: SchemeList, env: Env, cont: Continuation): SchemeData {
    return this.evaluator.evaluate(node.cadr(), env, this.getCont(node, env, cont))
  }

  private getCont(node: SchemeList, env: Env, cont: Continuation): Continuation {
    return new Continuation(
      (val: SchemeData) =>
        SchemeBoolean.isTrue(val)
          ? this.evaluator.evaluate(node.caddr(), env, cont)
          : this.evaluator.evaluate(node.cadddr(), env, cont)
    )

  }
}
