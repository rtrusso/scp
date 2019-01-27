(define (fact x)
  (if (= x 0)
      1
      (* x (fact (- x 1)))))

(display (fact 10))
(newline)
