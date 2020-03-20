(define (top-level-env)
  (let* ((env *special-forms*)
         (names '()))
;    (display "CREATING TOP-LEVEL ENV, NSPECIAL: ")
;    (write (length env))
;    (newline)
    (lambda (name . val)
      (define (tag-name! name)
        (cond ((assoc name names) => cdr)
              (else (let ((id (generate-unique-variable-id)))
                      (set! names (cons (cons name id) names))
                      id))))
      (define (bind binding)
        (if (symbol? binding)
            (free-variable binding (tag-name! binding))
            binding))
;      (display "top-level-env: ") (write name) (write val)
;      (write (null? val)) (newline)
      (cond ((null? val) 
             (cond ((assoc name env) => cdr)
                   (else (free-variable name (tag-name! name)))))
            (else (let ((obj (assoc name env)))
                    (if obj
                        (set-cdr! obj (bind (car val)))
                        (begin
                          (set! env (cons (cons name (bind (car val)))
                                          env))))))))))
