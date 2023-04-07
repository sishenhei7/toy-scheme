import { parse } from './parser'
import { Evaluator } from './evaluator'
import type { Thunk, SchemeData } from './parser/data'
import { Env } from './env'

export interface InterpreterOptions {
  log?: Function
  prompt?: Function
}
export default class Interpreter {
  private thunk: Thunk

  constructor(program: string, options?: InterpreterOptions) {
    const tokenList = parse(program)
    const evaluator = new Evaluator(options)
    this.thunk = evaluator.evaluateList(tokenList, new Env())
  }

  // trampoline
  public run(): string {
    let thunk: Thunk | SchemeData = this.thunk
    while (typeof thunk === 'function') {
      thunk = thunk()
    }
    return thunk.toString()
  }

  // public step(): Thunk | SchemeData {

  // }
}