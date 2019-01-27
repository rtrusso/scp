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

(test (vector->list #(1 2 3)))
(test (list->vector (list 1 2 3)))
(define x (make-vector 5 0))
(test x)
(test (vector-fill! x 3))
(test x)
(test (map (lambda (x) (+ x 1)) '(1 2 3 4 5)))
(define sum 0)
(test sum)
(test (for-each (lambda (x) (set! sum (+ x sum))) '(1 2 3 4 5)))
(test sum)
(test (call-with-input-file
          "tests/read-dat3.scm"
        (lambda (f)
          (list (input-port? f)
                (read f)))))
(test (call-with-output-file
          (vector-ref *argv* 1)
        (lambda (f)
          (write '(asdf) f)
          (output-port? f))))

;force
