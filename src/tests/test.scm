(define (factorial-iter i result)
  (if (= 0 i)
      result
      (factorial-iter (- i 1) (* result i))))
(display (factorial-iter 3 1))
