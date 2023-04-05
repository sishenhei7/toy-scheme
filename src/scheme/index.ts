import { parse } from './parser'
import { Evaluator } from './evaluator'
import program from './programs/n-queues'

export function test() {
  const evaluator = new Evaluator()
  console.log(evaluator)
  console.log('parse', parse(program).toString())
  console.log('===', evaluator.run(parse(program)))
}
