import { type SchemeData, type Continuation, SchemeSym, SchemeList, SchemeProc } from '../parser/data'
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

  public matches(value: string): boolean {
    return value === 'define'
  }

  public evaluate(node: SchemeList, env: Env, cont: Continuation): SchemeData {
    const varNode = node.cadr()
    const bodyNode = node.cddr()

    // 定义变量或者函数
    if (SchemeSym.matches(varNode)) {
      return env.define(varNode.value, this.evaluator.evaluate(bodyNode, env, cont))
    }

    // 定义函数
    if (SchemeList.matches(varNode)) {
      const name = SchemeSym.cast(varNode.car()).value
      return env.define(name, new SchemeProc(name, varNode.cdr(), bodyNode, env))
    }

    assert(false, 'Error: define clause evaluate error!')
  }
}
