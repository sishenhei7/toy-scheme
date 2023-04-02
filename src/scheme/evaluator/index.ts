import { type Cont, type SchemeData, SchemeCont, SchemeList, SchemeSym, SchemeProc } from '../parser/data'
import type { Env } from '../env'
import BuildInEvaluator from './buildin'
import LetEvaluator from './let'
import LambdaEvaluator from './lambda'
import IfEvaluator from './if'
import DefineEvaluator from './define'
import CondEvaluator from './cond'
import CallCCEvaluator from './call-cc'
import SetEvaluator from './set'
import BeginEvaluator from './begin'
import ProcEvaluator from './proc'

export interface IEvaluator {
  matches(value: string, env?: Env): boolean
  evaluate(node: SchemeData, env: Env, cont: Cont): SchemeData
}

export class Evaluator {
  private evaluators: IEvaluator[]
  private stepCount = 0
  private step = 0

  constructor() {
    this.evaluators = [
      new BuildInEvaluator(this),
      new LetEvaluator(this),
      new LambdaEvaluator(this),
      new IfEvaluator(this),
      new DefineEvaluator(this),
      new CondEvaluator(this),
      new CallCCEvaluator(this),
      new SetEvaluator(this),
      new BeginEvaluator(this),
      new ProcEvaluator(this)
    ]
  }

  public evaluateSchemeCont(node: SchemeCont) {
    while (SchemeCont.matches(node)) {
      // console.log(111111, node)
      node = node.call() as any
    }
  }

  public evaluate(node: SchemeData, env: Env, cont: Cont = SchemeCont.Identity): SchemeData {
    // is a sentence
    if (SchemeList.matches(node) && node.shouldEval) {
      const peek = node.car()

      if (SchemeSym.matches(peek)) {
        for (const evaluator of this.evaluators) {
          if (evaluator.matches(peek.value, env)) {
            return evaluator.evaluate(node, env, cont)
          }
        }
      }

      // 当节点是 SchemeCont 的时候，丢弃当前的 cont，直接执行 SchemeCont
      if (typeof peek === 'function' && !SchemeList.isNil(node.cdr())) {
        return this.evaluate(node.cadr(), env, peek)
      }

      return this.evaluateList(node, env, cont)
    }

    if (SchemeSym.matches(node)) {
      return new SchemeCont(cont, env.get(node.value))
    }

    return new SchemeCont(cont, node)
  }

  // 这里很重要，下一个语句是通过上一个语句的cont进行执行的！
  public evaluateList(node: SchemeList, env: Env, cont: Cont = SchemeCont.Identity): SchemeData {
    return this.evaluate(node.car(), env, (data: SchemeData) => {
      if (SchemeList.isNil(node.cdr())) {
        return new SchemeCont(cont, data)
      }
      return this.evaluate(node.cdr(), env, cont)
    })
  }

  // TODO: 缺少一个 application 的语法
  // public evaluateApplication(node: SchemeList, env: Env, cont: SchemeCont = SchemeCont.Identity) {

  // }
}
