(define a (make-string 10 #\a))
(display "[")
(display a)
(display "]")
(newline)
(string-set! a 5 #\c)
(display "[")
(display a)
(display "]")
(newline)
