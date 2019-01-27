(define-syntax test-syntax
  (syntax-rules (test-constant)
    ((_ <symbol> (test-constant <value-1> <value-2> ...))
     (define <symbol> (quote (list <value-1> <value-2> ...))))))
