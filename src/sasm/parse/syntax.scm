(need sasm/sasm-tracing)

(define-syntax sasm-syntax-case
  (syntax-rules (:pattern :rewrite)
    ((_ :pattern pattern
        :rewrite (symbol ...) body1 body2 ...)
     (lambda (expression)
       (let ((match (pattern-match (quasiquote pattern)
                                   expression)))
         (and match
              (apply (lambda (symbol ...)
                       body1 body2 ...)
                     (let ((map-result (map (lambda (sym)
                                              (let ((entry (assoc sym match)))
                                                (if entry
                                                    (cdr entry)
                                                    (error "Unmatched variable in sasm-syntax-case" sym))))
                                            '(symbol ...))))
                       map-result))
              ))))))

(define-syntax sasm-parse-error
  (syntax-rules (:error-message)
    ((_ :error-message <string> <operand> ...)
     (lambda (expression)
       (display <string>)
       (for-each display (list <operand> ...))
       (newline)
       (write expression)
       (newline)
       (newline)
       #f))))

