(define test-case-number 0)
(define-syntax test
  (syntax-rules ()
    ((_ expression)
     (begin (set! test-case-number (+ test-case-number 1))
            (display "case ")
            (display test-case-number)
            (display ": ")
            (newline)
            (write 'expression)
            (display " => ")
            (write expression)
            (newline)
            (newline)))))

(test (char-ci=? #\a #\A))
(test (char=? #\a #\A))
(test (char-ci<? #\a #\B))
(test (char-ci>? #\B #\a))
(test (char-ci<=? #\a #\A))
(test (char-ci<=? #\a #\B))
(test (char-ci>=? #\B #\b))
(test (char-alphabetic? #\a))
(test (char-alphabetic? #\0))
(test (char-numeric? #\0))
(test (char-whitespace? #\newline))
(test (char-lower-case? #\a))
(test (char-lower-case? #\A))
(test (char-upper-case? #\A))
(test (char-upper-case? #\a))
(test (char-upcase #\a))
(test (char-downcase #\A))

