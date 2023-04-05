import { parse } from './parser'
import { Evaluator } from './evaluator'
import { Env } from './env'
import program from './programs/factorial'

export function test() {
  const evaluator = new Evaluator()
  console.log(evaluator)
  console.log('parse', parse(program).toString())
  console.log('===', evaluator.run(parse(program)))
}
