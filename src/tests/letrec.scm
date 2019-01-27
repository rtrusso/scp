(define (foo x)
  (letrec ((a (list->string (list #\a #\b #\c)))
           (foo-1 (lambda (y) (+ x y)))
           (foo-2 (lambda (z) (+ (foo-1 z) 3)))
           (foo-3 (lambda (w) (+ (foo-2 w) 4))))
    (foo-3 9)))

(display (foo 17))
(newline)
