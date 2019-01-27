(define (scheme-codegen-variable-reference expr env target linkage)
  (define (get-lexical-address expr env up)
    (if (null? env)
        #f
        (let ((index (index-of expr eqv? (cdr env))))
          (if index
              (cons up index)
              (get-lexical-address expr (car env) (+ up 1))))))
  (define (codegen-lexical-lookup up over current-env)
    (if (< up 0) (error "invalid up value " up))
    (if (zero? up)
        (insn-seq (list (cadr current-env))
                  (target-modifies target)
                  `((assign ,target (op load-array) ,current-env (const ,(+ over 1)))))
        (append-insn-seq
         (insn-seq (list (cadr current-env))
                   '(accum)
                   `((assign (reg accum) (op load-array) ,current-env (const 0))))
         (codegen-lexical-lookup (- up 1) over '(reg accum)))))
  (let ((addr (get-lexical-address expr env 0)))
    (with-scheme-linkage
     (if addr
         (let ((label (make-label (string->symbol (string-append "lexical-reference-"
                                                                 (symbol->string expr)))
                                  (make-label-token))))
           (append-insn-seq
            (insn-seq '() '()
                      `((label ,label)))
            (codegen-lexical-lookup (car addr) (cdr addr) (env-register)))
           )
         (begin
           (if (not (member expr *top-level-referenced-symbols*))
               (set! *top-level-referenced-symbols*
                     (cons expr
                           *top-level-referenced-symbols*)))
           (insn-seq
            '()
            (target-modifies target)
            `((assign ,target
                      (op load-array)
                      (label ,(make-label-for-global expr))
                      (const 0))))))
     linkage)))

