import { type Thunk, SchemeCont, SchemeList, type SchemeData } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (cont xx)
 */

export default class ContEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) { }

  public matches(node: SchemeData): boolean {
    return SchemeCont.matches(node)
  }

  // 当节点是 SchemeCont 的时候，丢弃当前的 cont，直接执行 SchemeCont
  public evaluate(node: SchemeList, env: Env, _cont: SchemeCont): Thunk {
    return this.evaluator.evaluate(node.cadr(), env, SchemeCont.cast(node.car()))
  }
}
