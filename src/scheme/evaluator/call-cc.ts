import type { BaseData } from '../parser/data'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法（后面加一个 lambda 语句）：
 * (call-with-current-continuation
 *    (lambda (resume-here)
 *      (set! control-state resume-here)
 *      (return element)))
 */
export class CallCCEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(tag: string): boolean {
    return tag === 'call-with-current-continuation'
  }

  public evaluate(node: BaseData, env: Env, cont: Cont): BaseData {
    return node
  }
}
