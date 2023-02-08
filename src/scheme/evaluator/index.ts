import type { INode } from '../node'
import type { Env } from '../env'
import { SelfEvaluator } from './self';
import { VariableEvaluator } from './variable';
import { IfEvaluator } from './if'
import { DefineEvaluator } from './define';
import { CondEvaluator } from './cond';
import { CallCCEvaluator } from './call-cc';
import { SetEvaluator } from './set';
import { BeginEvaluator } from './begin';
import { QuoteEvaluator } from './quote';

export type Cont = (node: INode) => INode;

export interface IEvaluator {
  matches(): boolean
  evaluate(node: INode, env: Env, cont: Cont): INode
}

export class Evaluator {
  private evaluators: IEvaluator[]
  private stepCount = 0
  private step = 0

  constructor() {
    this.evaluators = [
      new SelfEvaluator(this),
      new VariableEvaluator(this),
      new IfEvaluator(this),
      new DefineEvaluator(this),
      new CondEvaluator(this),
      new CallCCEvaluator(this),
      new SetEvaluator(this),
      new BeginEvaluator(this),
      new QuoteEvaluator(this),
    ]
  }

  public evaluate(node: INode, env: Env, cont: Cont) {

  }
}
