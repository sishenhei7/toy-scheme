import { type SchemeData, Continuation, SchemeSym, SchemeProc, NodeData } from '../parser/data';
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

  public matches(tag: string, env: Env): boolean {
    return SchemeProc.matches(env.get(tag))
  }

  public evaluate(node: SchemeSym, env: Env, cont: Continuation): SchemeData {
    // 流程：
    // 1.建立env，连接parentStackFrame
    // 2.解析args到env里面去
    // 3.执行body
    // 4.把结果返回给cont

    // env 的作用：
    // 1.用来查找这个 proc
    // 2.用来查找 args 里面的变量
    const proc = env.get(node.tag)
    assert(SchemeProc.matches(proc), 'Evaluate proc error: not a SchemeProc!')

    const parentEnv = proc.envClosure // 词法作用域
    const parentStackframe = parentEnv?.getStackFrame() || null
    const newEnv = new Env(parentEnv, new StackFrame(parentStackframe)) // shade
    this.evaluateArgs(proc.params, node.next, env, newEnv)
    // TODO: 这里不能用 evaluateList，不然会报错，怎么办？
    return this.evaluator.evaluate(proc.body, newEnv, cont)
  }

  private evaluateArgs(params: NodeData | null, args: NodeData | null, env: Env, newEnv: Env): void {
    while (params && args) {
      assert(SchemeSym.matches(params), 'Proc params evaluate error!')

      const value = this.evaluator.evaluate(args, env)
      newEnv.define(params.tag, value) // 注意这里是 newEnv，不是 env
      params = params.next
      args = args.next
    }
    assert(!params && !args, 'Proc params and args do not match!')
  }
}
