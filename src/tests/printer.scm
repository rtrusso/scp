(define (make-printer x)
  (lambda (y)
    (display "was ")
    (display x)
    (display " now ")
    (display y)
    (newline)))

(define global-printer (make-printer 99))

