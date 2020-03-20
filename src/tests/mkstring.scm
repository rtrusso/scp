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
            (let ((result expression))
              (display " => ")
              (if (string? result)
                  (display "["))
              (display result)
              (if (string? result)
                  (display "]")))
            (newline)
            (newline)))))

(test (string? (make-string 10)))
(test (string-length (make-string 10)))
(test (make-string 10))
(test (make-string 10 #\a))