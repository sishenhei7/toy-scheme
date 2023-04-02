import { type Cont, type SchemeData, SchemeCont, SchemeSym, SchemeList, SchemeProc } from '../parser/data'
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

  public evaluate(node: SchemeList, env: Env, cont: Cont): SchemeData {
    const varNode = node.cadr()
    const bodyNode = node.cddr()

    // 定义变量或者函数
    if (SchemeSym.matches(varNode)) {
      return this.evaluator.evaluate(bodyNode, env, (data: SchemeData) => {
        env.define(varNode.value, data)
        return new SchemeCont(cont, data)
      })
    }

    // 定义函数
    if (SchemeList.matches(varNode)) {
      const name = SchemeSym.cast(varNode.car()).value
      const proc = new SchemeProc(name, varNode.cdr(), bodyNode, env)
      env.define(name, proc)
      return new SchemeCont(cont, proc)
    }

    assert(false, 'Error: define clause evaluate error!')
  }
}
