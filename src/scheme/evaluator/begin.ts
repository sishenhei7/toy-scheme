import type { SchemeData, Continuation, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

/**
 * 语法：
 * (begin
 *    (set! amount m)
 *    (set! amount n)
 *    amout)
 */
export default class BeginEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'begin'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    let currentNode = node.next
    assert(currentNode, 'Syntax error: begin followed no clause!')
    return this.evaluator.evaluateList(currentNode, env, cont)
  }
}
