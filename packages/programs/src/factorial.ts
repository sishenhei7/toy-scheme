export default `; Factorial
; 500000 should have exceeded maximum call stack size,
; but my scheme interpreter have implemented trampoline,
; so the stack size won't grow

(define (factorial x)
    (cond
        ((= x 1) 1)
        (else (+ 1 (factorial (- x 1))))))

(display (factorial 500))
; support factorial 500000 here
`
