
(define (expand-context2 type key-value)
  (lambda (name)
    (if (eqv? name 'type)
        type
        (let loop ((rest key-value))
          (cond ((null? rest)
                 (error "Invalid context query name" type name))
                ((eqv? name (car (car rest)))
                 (car (cdr (car rest))))
                (else
                 (loop (cdr rest))))))))

;; (define-syntax expand-context
;;   (syntax-rules ()
;;     ((_ <type> (<name> <value>) ...)
;;      (lambda (name)
;;        (case name
;;          ((type) '<type>)
;;          ((<name>) <value>)
;;          ...
;;          (else (error "Invalid context query name" '<type> name)))))))
