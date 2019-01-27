(define test-case-number 0)
(define-syntax test
  (syntax-rules ()
    ((_ expression)
     (begin (set! test-case-number (+ test-case-number 1))
            (display "case ")
            (display test-case-number)
            (display ": ")
            (newline)
            (display 'expression)
            (display " => ")
            (display expression)
            (newline)
            (newline)))))

(test (* 2))
(test (* 2 3))
(test (* 2 3 4 5 6))
(test (* -1 -1))
(test (* -1 3 -1))
