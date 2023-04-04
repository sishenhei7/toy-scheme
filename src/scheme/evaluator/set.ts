import { type Thunk, type SchemeData, SchemeCont, SchemeSym, SchemeList } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法：
 * (set! var (* var 10))
 */
export default class SetEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(node: SchemeData): boolean {
    return SchemeSym.matches(node) && node.value === 'set!'
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    return this.evaluator.evaluate(
      node.caddr(),
      env,
      new SchemeCont((val: SchemeData) => {
        env.set(SchemeSym.cast(node.cadr()).value, val)
        return cont.call(val)
      })
    )
  }
}
