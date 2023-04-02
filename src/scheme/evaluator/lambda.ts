import { type Cont, SchemeCont, type SchemeData, SchemeList, SchemeProc } from '../parser/data'
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

  public evaluate(node: SchemeList, env: Env, cont: Cont): SchemeData {
    return new SchemeCont(cont, new SchemeProc('<<lambda>>', SchemeList.cast(node.cadr()), node.caddr(), env))
  }
}
