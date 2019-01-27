(define (quotient2 numerator denominator)
  (define (sign x)
    (cond ((zero? x)
           0)
          ((negative? x)
           -1)
          (else
           1)))

  (define (iter-fast result remainder denominator depth)
    (let ((next (* denominator depth)))
      (if (< next remainder)
          (iter-fast (+ result depth)
                     (- remainder next)
                     denominator
                     next)
          (iter-slow result remainder denominator))))

  (define (iter-slow result remainder denominator)
    (if (< remainder denominator)
        result
        (iter-fast (+ result 1) (- remainder denominator) denominator 1)))

  (let ((result (iter-fast 0 (abs numerator) (abs denominator) 1)))
    (if (= (sign numerator) (sign denominator))
        result
        (- 0 result))))

(display (quotient2 109 3)) ; 36
(newline)
(display (quotient2 1523932893 10)) ; 152393289
(newline)
