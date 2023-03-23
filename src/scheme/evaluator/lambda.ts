import { type Continuation, type SchemeData, SchemeSym, SchemeExp, SchemeProc } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

/**
 * 语法：
 * (lambda (a b c) (+ a b c))
 */
export default class LambdaEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'lambda'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    assert(node.next && node.next.next, 'Lambda evaluting error: should follow 2 expressions!')
    const params = SchemeExp.matches(node.next) ? node.next.body : node.next
    const body = SchemeExp.matches(node.next.next) ? node.next.next.body : node.next.next
    assert(body, 'Lambda evaluting error: body should exist!')
    return new SchemeProc('<<lambda>>', params, body, env)
  }
}
