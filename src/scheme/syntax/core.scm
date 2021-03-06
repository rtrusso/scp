(need util/string)
(need util/vector)
(need scheme/syntax/expand-context)
(need scheme/syntax/core/specform)
(need scheme/syntax/core/macro)
(need scheme/syntax/core/reference)
(need scheme/syntax/core/expand)
(need scheme/syntax/core/env)
(need scheme/syntax/core/context)

(define (expand-immediate obj env ctx)
  (define (fix obj)
    (cond ((generated-name? obj) (generated-name-symbol obj))
          ((pair? obj) (cons (fix (car obj)) (fix (cdr obj))))
          ((vector? obj) (vector-map fix obj))
          (else obj)))
  (fix obj))

(define (immediate? expression)
  (or (vector? expression)
      (number? expression)
      (char? expression)
      (string? expression)
      (boolean? expression)))

(define (quoted-immediate? expression)
  (and (pair? expression)
       (eqv? 'quote (car expression))
       (pair? (cdr expression))
       (equal? (cddr expression) '())))

(define (quoted-expression expression)
  (cadr expression))

(define (name? expression)
  (symbol? expression))

(define (free-variable name id)
  (list 'free name id))

(define (free? binding)
  (and (list? binding)
       (= 3 (length binding))
       (eqv? 'free (car binding))))

(define (bad-binding? env name)
  (let ((binding (bound-value env name)))
    (and (not (free? binding))
         (generated-name? (list-ref binding 1)))))

(define (check-bad-binding context env name)
  (let ((binding (bound-value env name)))
    (if (and (not (free? binding))
             (generated-name? (list-ref binding 1)))
        (error (string-append "invalid binding "
                              context)
               (list-ref binding 1)))))

(define (binding->global binding)
  (define (rj pad s n)
    (if (>= (string-length s) n)
        s
        (rj pad (string-append pad s) n)))
;  (display "binding->global ") (write binding) (newline)
  (if (free? binding)
      (list-ref binding 1)
      (string->symbol (string-append (symbol->string (car binding))
                                     "_"
                                     (rj "0"
                                         (number->string (list-ref binding 2))
                                         8)
                                     "@"
                                     (let ((sym (list-ref binding 1)))
                                       (if (not (symbol? sym))
                                           (error "invalid binding " (list-ref binding 1)))
                                       (if (generated-name? sym)
                                           (error "invalid binding, generated name " (list-ref binding 1)))
                                       (symbol->string sym))))))

(define (rename-variable env name)
  (check-bad-binding "rename-variable" env name)
  (binding->global (bound-value env name)))

