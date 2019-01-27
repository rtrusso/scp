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

(test (/ 1))
(test (/ 2))
(test (/ 180000))
(test (/ 100000 10))
(test (/ 113 13))
(test (/ 729 27))
(test (/ 200 2))
(test (/ 2 4))
(test (/ 2 1))
(test (/ 180000 1))
(test (/ 314159 1300))
(test (/ 2048 2 2 2 2 2 2 2))
