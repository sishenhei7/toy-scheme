import { type BaseData, type Cont, type SchemeData, SchemeBoolean } from '../parser/data'
import type { Env } from '../env'
import { assert } from '../utils'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (if predicate then_value else_value)
 */
export class IfEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'if'
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): SchemeData {
    return this.evaluator.evaluate(this.getPredicate(node), env, this.getCont(node, env, cont))
  }

  private getCont(node: BaseData, env: Env, cont: Cont): Cont {
    return (val: SchemeData) =>
      SchemeBoolean.isTrue(val)
        ? this.evaluator.evaluate(this.getThen(node), env, cont)
        : this.evaluator.evaluate(this.getElse(node), env, cont)
  }

  private getPredicate(node: BaseData): BaseData {
    assert(node?.next, 'Syntax error: if clause need predicate clause!')
    return node.next
  }

  private getThen(node: BaseData): BaseData {
    assert(node?.next?.next, 'Syntax error: if clause need then clause!')
    return node.next.next
  }

  private getElse(node: BaseData): BaseData {
    assert(node?.next?.next?.next, 'Syntax error: if clause need else clause!')
    return node.next.next.next
  }
}
