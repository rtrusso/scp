(define (factorial n)
  (define (iter i result)
    (if (= 0 i)
        result
        (iter (- i 1) (* result i))))
  (iter n 1))
(display (factorial 3))
