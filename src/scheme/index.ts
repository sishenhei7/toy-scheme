import { Parser } from "./parser";

const program = `
  ; Factorial

  (define (factorial x)
      (cond
          ((= x 1) 1)
          (else (* x (factorial (- x 1))))))

  (display (factorial 5))

`

export function test() {
  const parser = new Parser()
  console.log(parser.parse(program))
}
