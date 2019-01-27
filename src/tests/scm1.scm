(define (sum a b)
  (define (iter sum i)
    (if (< i b)
        (iter (+ sum i) (+ i 1))
        sum))
  (iter 0 a))

(display (sum 0 10))
(newline)

