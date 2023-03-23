import type { SchemeData, Continuation, SchemeSym } from '../parser/data'
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

  public matches(tag: string): boolean {
    return tag === 'call-with-current-continuation'
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    return cont
  }
}
