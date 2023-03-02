import type { SchemeSym, Cont } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator } from './index'

/**
 * 语法(直接是一个变量，有可能是一个函数，或者continuation)：
 * x
 */
export class VariableEvaluator implements IEvaluator {
  constructor() { }

  public matches(): boolean {
    return true
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): SchemeData {
    return cont(env.get(node.tag))
  }
}
