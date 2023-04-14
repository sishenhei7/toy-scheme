export default `; Factorial
; 500000 should have exceeded maximum call stack size,
; but my scheme interpreter have implemented trampoline,
; so the stack size won't grow

; 3
(define (test a) (+ a 1))
(display (test 2))
(display "\n")

; <<function>>
(display (lambda () (+ 2 1)))
(display "\n")

; 3
(display ((lambda (a) (+ a 1)) 2))
(display "\n")

; 3
(display ((lambda () (+ 2 1))))
`
