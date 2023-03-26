import type { SchemeData, Continuation, SchemeList } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator } from './index'

/**
 * 语法（后面加一个 lambda 语句）：
 * (call-with-current-continuation
 *    (lambda (resume-here)
 *      (set! control-state resume-here)
 *      (return element)))
 */
export default class CallCCEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(value: string): boolean {
    return value === 'call-with-current-continuation'
  }

  public evaluate(node: SchemeList, env: Env, cont: Continuation): SchemeData {
    return cont
  }
}
