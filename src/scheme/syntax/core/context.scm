(define (top-level-context)
  (let ((properties '()))
    (lambda (name . args)
      (case name
        ((type) 'expand-context/top-level)
        ((getprop) (if (not (and (= 1 (length args))
                                 (symbol? (car args))))
                       (error "Expand context getprop expects 1 symbol arg"
                              args)
                       (assoc (car args) properties)))
        ((setprop)
         (if (not (and (= 2 (length args))
                       (symbol? (car args))))
             (error "Invalid args to context setprop" args)
             (let* ((sym (car args))
                    (obj (cadr args))
                    (rec (assoc sym properties)))
               (if rec
                   (set-cdr! rec obj)
                   (set! properties (cons (cons sym obj) properties))))))
        (else (error "Invalid context query" name args))))))

(define (in-expand-context? ctx-actual ctx-name-required)
  (eqv? (ctx-actual 'type) ctx-name-required))

(define (expand-context-getprop ctx prop)
  (ctx 'getprop prop))

(define (expand-context-setprop ctx prop obj)
  (ctx 'setprop prop obj))

