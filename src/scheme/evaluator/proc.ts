import { type Thunk, type SchemeData, SchemeCont, SchemeSym, SchemeProc, SchemeList } from '../parser/data';
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

  public matches(_: SchemeData): boolean {
    return true
  }

  public evaluate(node: SchemeList, env: Env, cont: SchemeCont): Thunk {
    // env 的作用：
    // 1.用来查找这个 proc
    // 2.用来查找 args 里面的变量
    return this.evaluator.evaluate(node.car(), env, new SchemeCont((data: SchemeData) => {
      assert(SchemeProc.matches(data), 'Application error: not a valid SchemeProc!')
      return this.evaluateProc(data, node.cdr(), env, cont)
    }))
  }

  public evaluateProc(proc: SchemeProc, args: SchemeList, env: Env, cont: SchemeCont): Thunk {
    // 流程：
    // 1.建立env，连接parentStackFrame
    // 2.解析args到env里面去
    // 3.执行body
    // 4.把结果返回给cont
    const parentEnv = proc.envClosure // 词法作用域
    const newEnv = new Env(parentEnv, new StackFrame(parentEnv.getStackFrame())) // shade
    return this.evaluateArgs(
      proc.params,
      args,
      env,
      newEnv,
      new SchemeCont((_: SchemeData) => this.evaluator.evaluateList(proc.body, newEnv, cont))
    )
  }

  private evaluateArgs(params: SchemeList, args: SchemeList, env: Env, newEnv: Env, cont: SchemeCont): Thunk {
    if (!SchemeList.isNil(params)) {
      assert(!SchemeList.isNil(args), 'Proc params and args do not match!')
      return this.evaluator.evaluate(args.car(), env, new SchemeCont((data: SchemeData) => {
        const name = SchemeSym.cast(params.car()).value
        newEnv.setCurrent(name, data)
        return this.evaluateArgs(params.cdr(), args.cdr(), env, newEnv, cont)
      }))
    }
    return cont.call(SchemeList.buildSchemeNil())
  }
}
