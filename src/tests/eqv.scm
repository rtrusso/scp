(display (eqv? 3 2))
(newline)
(display (eqv? 2 2))
(newline)
(display (eqv? 3 "asdf"))
(newline)

(newline)

(display (eqv? 'jkl 3))
(newline)
(display (eqv? 'jkl 'jkl))
(newline)
(display (eqv? 'asdf 'jkl))
(newline)

(newline)

(display (eqv? #t #t))
(newline)
(display (eqv? #t #f))
(newline)
(display (eqv? #f #f))
(newline)

(newline)

(define v (make-vector 10 0))
(define l (list 1 2 3))
(display (eqv? (vector-set! v 0 1) 3))
(newline)
(display (eqv? l l))
(newline)
(display (eqv? (set-car! l 7) 7))
(newline)
(display v)
(newline)
(display l)
(newline)

(newline)

(define x #f)
(display (eqv? (set! x #t) #t))
(newline)
(display (eqv? #t 3))
(newline)
(display (eqv? (set! x 3) 3))
(newline)

(newline)

(display (eqv? (if #t #t #f) #t))
(newline)
(display (eqv? (if #f #t #f) #t))
(newline)
(display (eqv? (if #t 3 4) 3))
(newline)

(newline)

(display (eqv? (null? (if #f #t)) #t))
(newline)
(display (eqv? (if #f #t) #t))
(newline)
(display (eqv? (null? '()) (null? (cdr '(1)))))
(newline)

(newline)
(define y (+ 7 9))
(define z (+ 10 11))
(display (eq? (set! y (+ y 1))
              (set! z (+ z 2))))
(newline)
