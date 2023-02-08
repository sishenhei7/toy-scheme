import type { INode } from '../node'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

/**
 * 语法：
 * (call-with-current-continuation
 *    (lambda (resume-here)
 *      (set! control-state resume-here)
 *      (return element)))
 */
export class CallCCEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) { }

  public matches(): boolean {
    return false
  }

  public evaluate(node: INode, env: Env, cont: Cont): INode {
    return node
  }
}