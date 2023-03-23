import { type NodeData, type SchemeData, Continuation, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import { assert } from '../utils'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (set! var (* var 10))
 */
export default class SetEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'set!'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    return this.evaluator.evaluate(
      this.getValue(node),
      env,
      new Continuation((val: SchemeData) => {
        env.set(this.getVar(node), val)
        return cont.call(val)
      })
    )
  }

  private getVar(node: SchemeSym): string {
    assert(node?.next, 'Syntax error: set clause need variation!')
    assert(SchemeSym.matches(node.next), 'Syntax error: variation of set clause should be string!')
    return node.next.tag
  }

  private getValue(node: SchemeSym): NodeData {
    assert(node?.next?.next, 'Syntax error: set clause need value!')
    return node.next.next
  }
}
