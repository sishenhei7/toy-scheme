import type { SchemeData, Cont, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * 1.定义变量或函数：
 * (define vhello "Hello world")
 * 2.短形式定义函数：
 * (define (hello name) (string-append "Hello " name "!"))
 */

export class DefineEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'define'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    // TODO: ts-error
    // @ts-expect-error
    return node
  }
}
