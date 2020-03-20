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
            expression
            (newline)
            (newline)))))

(test (write "asdf"))
(test (write 'a))
(test (write (make-vector 3 0)))
(test (write "asdf" (current-output-port)))
(test (write (make-vector 3 0) (current-output-port)))
(test (write "asdf" (current-output-port) (current-output-port)))
(test (write #\newline))
(test (write '("asdf" jkl 3 5 7 #\0 #\z)))