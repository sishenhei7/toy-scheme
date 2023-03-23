export default `
; 8-queens
; How many ways can we place eight chess queens on an 8×8 chessboard so that no two queens threaten each other?

(define (backtrack legal-pl boardsize)
    (cond
      ((null? legal-pl) '())
      (else 2))
`