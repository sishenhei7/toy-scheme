import type { BaseData } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法：
 * (if predicate then_value else_value)
 */
export class IfEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(): boolean {
    return false
  }

  public applyCont(cont: Cont, env: Env, val: any) {
    // const thenExp =
  }

  public getCont(node: BaseData, env: Env, cont: Cont): Cont {
    const thenNode = node.next?.next
    const elseNode = node.next?.next?.next
    return (val) =>
      val
        ? this.evaluator.evaluate(thenNode as any, env, cont)
        : this.evaluator.evaluate(elseNode as any, env, cont)
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    const newCont = this.getCont(node, env, cont)
    return this.evaluator.evaluate(node.next as any, env, newCont)
  }

  private getPredicate(node: BaseData) {
    return node.next
  }
  private getThenValue(node: BaseData) {
    return node.next?.next
  }
  private getElseValue(node: BaseData) {
    return node.next?.next?.next
  }
}
