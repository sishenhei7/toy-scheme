import { SchemeSym, type Continuation, type SchemeData, SchemeNumber, SchemeString, SchemeBoolean } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator } from './index'

/**
 * 语法(直接是一个变量，有可能是一个函数，或者continuation)：
 * x
 */
export default class VariableEvaluator implements IEvaluator {
  constructor() { }

  public matches(tag: string, env: Env): boolean {
    const value = env.get(tag)
    return SchemeNumber.matches(value)
      || SchemeString.matches(value)
      || SchemeBoolean.matches(value)
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    return cont.call(env.get(node.value))
  }
}
