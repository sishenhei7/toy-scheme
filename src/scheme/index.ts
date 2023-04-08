import { parse } from './parser'
import { Evaluator } from './evaluator'
import { type SchemeData, SchemeCont, type ILocationRange } from './parser/data'
import { Env } from './env'

export interface InterpreterOptions {
  log?: Function
  prompt?: Function
}
export interface StepResponse {
  range: ILocationRange | null
  env: Env | null
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
      console.log(node)
      node = node.call()
    }
    return node.toString()
  }

  public step(): StepResponse {
    const { node } = this
    this.node = SchemeCont.matches(node) ? node.call() : node
    console.log(this.node)
    return {
      range: this.node.range,
      env: this.node.getEnv()
    }
  }
}