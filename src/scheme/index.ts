import { parse } from './parser'
import { Evaluator } from './evaluator'
import { Env } from './env'
import program from './programs/return-with-callcc'

export function test() {
  const evaluator = new Evaluator()
  console.log(evaluator)
  console.log('parse', parse(program).toString())
  console.log('===', evaluator.evaluateSchemeCont(evaluator.evaluateList(parse(program), new Env()) as any))
}
