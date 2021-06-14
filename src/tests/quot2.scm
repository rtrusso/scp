(define (quotient2 numerator denominator)
  (define (sign x)
    (cond ((zero? x)
           0)
          ((negative? x)
           -1)
          (else
           1)))

  (define (iter-fast result remainder denominator depth last-depth)
    (display (list 'iter-fast result remainder denominator depth last-depth))
    (newline)
    (let ((next (* denominator depth)))
      (if (> next depth) ; this is a multiply overflow, or denom=1
          (if (< next remainder)
              (iter-fast (+ result depth)
                         (- remainder next)
                         denominator
                         next
                         depth)
              (iter-slow result remainder denominator))
          (if (and (> last-depth 0) (< depth remainder))
              (iter-fast (+ result last-depth) ; depth can't get bigger, so keep iterating at current depth
                         (- remainder depth)
                         denominator
                         depth
                         last-depth)
              (iter-slow result remainder denominator)
              ))))

  (define (iter-slow result remainder denominator)
    (if (< remainder denominator)
        (begin (display (list 'iter-slow-result result))
               (newline)
               result)
        (iter-fast (+ result 1) (- remainder denominator) denominator 1 0)))

  (if (zero? denominator)
      (error "quotient2 - zero denominator"))
  (if (= 1 denominator)
      numerator
      (let ((result (iter-fast 0 (abs numerator) (abs denominator) 1 0)))
        (if (= (sign numerator) (sign denominator))
            result
            (- 0 result)))))

(display (quotient2 2 1))
(newline)
(display (quotient2 18000 1))
(newline)
