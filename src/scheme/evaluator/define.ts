import { type SchemeData, type Cont, SchemeSym, SchemeExp, SchemeProc } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

/**
 * 语法：
 * 1.定义变量或函数：
 * (define vhello "Hello world")
 * 2.短形式定义函数：
 * (define (hello name) (string-append "Hello " name "!"))
 */

export default class DefineEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'define'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    assert(!!node.next && !!node.next.next, 'Define evaluating error: args must be two!')

    const varNode = node.next
    const bodyNode = node.next.next

    // 定义变量或者函数
    if (SchemeSym.matches(varNode)) {
      return env.define(varNode.tag, this.evaluator.evaluate(bodyNode, env, cont))
    }

    // 定义函数
    if (SchemeExp.matches(varNode) && SchemeExp.matches(bodyNode)) {
      assert(varNode.body && SchemeSym.matches(varNode.body), 'Define evaluating error: no proc name!')
      const name = varNode.body.tag
      const params = varNode.body.next
      const body = bodyNode.body
      assert(body, 'Define evaluting error: proc body should exist!')
      return env.define(name, new SchemeProc(name, params, body, env))
    }

    assert(true, 'Error: define clause evaluate error!')
    return null as never
  }
}
