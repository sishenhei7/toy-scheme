import { type Continuation, type SchemeData, SchemeList, SchemeProc } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (lambda (a b c) (+ a b c))
 */
export default class LambdaEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(value: string): boolean {
    return value === 'lambda'
  }

  public evaluate(node: SchemeList, env: Env, cont: Continuation): SchemeData {
    return cont.call(new SchemeProc('<<lambda>>', SchemeList.cast(node.cadr()), node.caddr(), env))
  }
}
