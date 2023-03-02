import type { SchemeData, Cont, SchemeSym } from '../parser/data'
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
export class BeginEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'begin'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    let currentNode = node.next
    assert(currentNode, 'Syntax error: begin followed no clause!')

    let res = null
    while (currentNode) {
      res = this.evaluator.evaluate(currentNode, env, cont)
      currentNode = currentNode.next
    }

    assert(res, 'Error: begin clause evaluate error!')
    return res
  }
}
