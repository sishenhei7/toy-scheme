import { parse } from './parser'
import { Evaluator } from './evaluator'
import { type SchemeData, SchemeCont, type ILocationRange } from './parser/data'
import { Env } from './env'
import { nextTick } from './utils'

export interface InterpreterOptions {
  log?: Function
  prompt?: Function
}
export interface StepResponse {
  range: ILocationRange | null
  stack: string[]
  scope: string[]
}
export default class Interpreter {
  private node: SchemeData

  constructor(program: string, options?: InterpreterOptions) {
    const tokenList = parse(program)
    const evaluator = new Evaluator(options)
    this.node = evaluator.evaluateList(tokenList, new Env())
  }

  // trampoline
  public run(): string {
    let node = this.node
    while (SchemeCont.matches(node)) {
      // console.log(node)
      node = node.call()
    }
    return node.toString()
  }

  public smoothRun(callback?: Function, checkStop?: Function) {
    const loop = (n: number) => {
      if (checkStop && checkStop()) {
        callback && callback()
        return
      }

      const { node } = this
      if (SchemeCont.matches(node)) {
        // console.log(node)
        this.node = node.call()

        if (n >= 0) {
          n -= 1
          loop(n)
        } else {
          this.smoothRun(callback, checkStop)
        }
      } else {
        callback && callback()
      }
    }

    // 每次 nexttick 调用自身 30 次
    nextTick(() => loop(30))
  }

  public step(): StepResponse {
    const { node } = this
    this.node = SchemeCont.matches(node) ? node.call() : node
    const env = this.node.getEnv()
    console.log(this.node)
    return {
      range: this.node.range,
      stack: this.getStack(env),
      scope: env ? env.getVarScope() : []
    }
  }

  private getStack(env: Env | null): string[] {
    let res: string[] = []

    if (!env) {
      return res
    }

    let stackFrame = env.getStackFrame()
    while (stackFrame) {
      res.push(stackFrame.toString())
      stackFrame = stackFrame.getParent()
    }
    return res
  }
}