(define x (call-with-input-file "tests/read-dat-imp.scm"
            (lambda (f) (read f))))

(display "x: ")
(write x)
(newline)

(display "pair? ")
(display (pair? x))
(newline)

(display "car - symbol? ")
(display (symbol? (car x)))
(newline)

(display "cdr - symbol? ")
(display (symbol? (cdr x)))
(newline)
