import type { INode } from '../node'
import type { Env } from '../env'
import type { IEvaluator, Evaluator, Cont } from './index'

export class LambdaEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) { }

  public matches(): boolean {
    return false
  }

  public evaluate(node: INode, env: Env, cont: Cont): INode {
    return node
  }
}