(define (special-form? obj)
  (and (list? obj) (= (length obj) 3) (eqv? 'specform (car obj))))
(define *special-forms* '())
(define (install-special-form keyword expander)
  (set! *special-forms*
        (cons (cons keyword (list 'specform keyword expander))
              *special-forms*)))
(define (expand-special-form specform expression environment context)
  ((list-ref specform 2) expression environment context))

