import { type SchemeData, type Cont, type SchemeSym, SchemeFunction } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * 函数调用：
 * (vhello "Hello world")
 */

export class ProcEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string, env: Env): boolean {
    return SchemeFunction.matches(env.get(tag))
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    // TODO: ts-error
    // @ts-expect-error
    return node
  }
}
