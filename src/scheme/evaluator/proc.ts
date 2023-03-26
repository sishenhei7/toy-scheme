import { type SchemeData, Continuation, SchemeSym, SchemeProc, SchemeList } from '../parser/data';
import { Env, StackFrame } from '../env'
import type { IEvaluator, Evaluator } from './index'
import { assert } from '../utils'

/**
 * 语法：
 * 函数调用：
 * (vhello "Hello world")
 */

export default class ProcEvaluator implements IEvaluator {
  constructor(private evaluator: Evaluator) {}

  public matches(value: string, env: Env): boolean {
    return SchemeProc.matches(env.get(value))
  }

  public evaluate(node: SchemeList, env: Env, cont: Continuation): SchemeData {
    // 流程：
    // 1.建立env，连接parentStackFrame
    // 2.解析args到env里面去
    // 3.执行body
    // 4.把结果返回给cont

    // env 的作用：
    // 1.用来查找这个 proc
    // 2.用来查找 args 里面的变量
    const proc = env.get(SchemeSym.cast(node.car()).value)
    assert(SchemeProc.matches(proc), 'Evaluate proc error: not a SchemeProc!')

    const parentEnv = proc.envClosure // 词法作用域
    const parentStackframe = parentEnv.getStackFrame()
    const newEnv = new Env(parentEnv, new StackFrame(parentStackframe)) // shade
    this.evaluateArgs(proc.params, node.cdr(), env, newEnv)
    return this.evaluator.evaluate(proc.body, newEnv, cont)
  }

  private evaluateArgs(params: SchemeList, args: SchemeList, env: Env, newEnv: Env): void {
    while (!SchemeList.isNil(params) && !SchemeList.isNil(args)) {
      const value = this.evaluator.evaluate(args.car(), env)
      newEnv.define(SchemeSym.cast(params.car()).value, value) // 注意这里是 newEnv，不是 env
      params = params.cdr()
      args = args.cdr()
    }
    assert(!params && !args, 'Proc params and args do not match!')
  }
}
