(define (fun1)
  (display "fun1")
  (newline)
  1)

(define (fun2)
  (display "fun2")
  (newline)
  2)

(define (fun3)
  (display "fun3")
  (newline)
  3)

(write (vector (fun1) (fun2) (fun3)))
(newline)
