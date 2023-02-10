import { type BaseData, SchemeBoolean } from '../parser/data';
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
    const predicateNode = this.getPredicate(node)
    const thenNode = this.getThenValue(node)
    const elseNode = this.getElseValue(node)
    return this.evaluator.evaluate(predicateNode, env, (val: BaseData) => {
      return SchemeBoolean.isTrue(val)
        ? this.evaluator.evaluate(thenNode, env, cont)
        : this.evaluator.evaluate(elseNode, env, cont)
    })
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
