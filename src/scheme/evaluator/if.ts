import { type BaseData, SchemeBoolean } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法：
 * (if predicate then_value else_value)
 */
export class IfEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'if'
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return this.evaluator.evaluate(this.getPredicate(node), env, this.getCont(node, env, cont))
  }

  private getCont(node: BaseData, env: Env, cont: Cont): Cont {
    return (val: BaseData) =>
      SchemeBoolean.isTrue(val)
        ? this.evaluator.evaluate(this.getThenValue(node), env, cont)
        : this.evaluator.evaluate(this.getElseValue(node), env, cont)
  }

  private getPredicate(node: BaseData): BaseData {
    if (!node?.next) {
      throw Error('Syntax error: if clause need predicate clause!')
    }
    return node.next
  }

  private getThenValue(node: BaseData): BaseData {
    if (!node?.next?.next) {
      throw Error('Syntax error: if clause need then clause!')
    }
    return node.next.next
  }

  private getElseValue(node: BaseData): BaseData {
    if (!node.next?.next?.next) {
      throw Error('Syntax error: if clause need else clause!')
    }
    return node.next.next.next
  }
}
