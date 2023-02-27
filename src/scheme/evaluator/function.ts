import { type SchemeData, type Cont, SchemeFunction } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * 函数调用：
 * (vhello "Hello world")
 */

export class FunctionEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) { }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return node
  }
}
