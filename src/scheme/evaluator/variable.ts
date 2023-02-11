import type { BaseData, SchemeSym } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法(直接是一个变量)：
 * x
 */
export class VariableEvaluator implements IEvaluator {
  constructor() { }

  public matches(): boolean {
    return true
  }

  public evaluate(node: SchemeSym, env: Env, cont: Cont): BaseData {
    return cont(env.get(node.tag))
  }
}
