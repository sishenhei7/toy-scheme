import { type BaseData, type Cont, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import { assert } from '../utils'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (set! var (* var 10))
 */
export class SetEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'set!'
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return this.evaluator.evaluate(this.getValue(node), env, (val: BaseData) => {
      env.set(this.getVar(node), val)
      return cont(val)
    })
  }

  private getVar(node: BaseData): string {
    assert(node?.next, 'Syntax error: set clause need variation!')
    assert(SchemeSym.matches(node.next), 'Syntax error: variation of set clause should be string!')
    return node.next.tag
  }

  private getValue(node: BaseData): BaseData {
    assert(node?.next?.next, 'Syntax error: set clause need value!')
    return node.next.next
  }
}
