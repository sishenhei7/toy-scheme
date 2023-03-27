import { parse } from './parser'
import { Evaluator } from './evaluator'
import { Env } from './env'
import program from './programs/n-queues'

export function test() {
  const evaluator = new Evaluator()
  console.log(evaluator)
  console.log(parse(program).toString())
  console.log('===', evaluator.evaluateList(parse(program), new Env()))
}
