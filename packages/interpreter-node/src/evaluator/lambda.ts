import { SchemeCont, type SchemeData, SchemeList, SchemeProc, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator } from './index'

/**
 * 语法：
 * (lambda (a b c) (+ a b c))
 */
export default class LambdaEvaluator implements IEvaluator {
  constructor() {}

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'lambda'
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): SchemeData {
    return cont
      .setValue(new SchemeProc('<<lambda>>', SchemeList.cast(node.cadr()), node.cddr(), env))
      .setEnv(env)
      .setLocationInfo(node.range)
  }
}
