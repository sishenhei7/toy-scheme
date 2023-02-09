import { parse } from './parser'
import { Evaluator } from './evaluator'

const program = `
  ; Factorial

  (define (factorial x)
      (cond
          ((= x 1) 1)
          (else (* x (factorial (- x 1))))))

  (display (factorial 5))

`

export function test() {
  const evaluator = new Evaluator()
  console.log(evaluator)
  console.log(parse(program))
}
