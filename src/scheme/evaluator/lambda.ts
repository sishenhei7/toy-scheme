import type { BaseData, Cont } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (lambda (a b c) (+ a b c))
 */
export class LambdaEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'lambda'
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return node
  }

  // private getArgs(node: BaseData) {

  // }

  // private getBody(node: BaseData) {

  // }
}
