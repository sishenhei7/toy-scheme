export default `; Tower of Hanoi

(define (tower-of-hanoi n source dest temp)
  (if (= n 1)
      (begin
        (display "Move disk-" n " from " source " to " dest "\n"))
      (begin
        (tower-of-hanoi (- n 1) source temp dest)
        (display "Move disk-" n " from " source " to " dest "\n")
        (tower-of-hanoi (- n 1) temp dest source))))

(tower-of-hanoi (ask "Number of disks? [1-50]") "source" "dest" "temp")
`
