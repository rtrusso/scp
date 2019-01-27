(define (scheme-codegen-letrec expr env target linkage)
  (define (load-vars idx exps env insns)
    (if (null? exps)
        insns
        (load-vars (+ idx 1) (cdr exps) env
                   (preserving
                    (scheme-preserved-registers)
                    insns
                    (preserving (scheme-preserved-registers)
                                (scheme-codegen (car exps) env '(reg accum)
                                                (next-linkage))
                                (insn-seq `(accum ,(cadr (env-register)))
                                          '()
                                          `((perform (op store-array)
                                                     ,(env-register)
                                                     (const ,idx)
                                                     (reg accum)))))))))

  (define (compile-body body env)
    (scheme-codegen (cons 'begin body) env target linkage))
  (let ((var-names (letrec-vars expr))
        (exprs (letrec-exps expr))
        (body (letrec-body expr))
        (sub-env (cons env (letrec-vars expr))))
    (append-insn-seq
     (insn-seq
      `(,(cadr (env-register)) ,(cadr (link-register)))
      '(accum index operand ,(cadr (env-register)))
      `((save ,(env-register))
        (save ,(link-register))
        (push (const ,(+ 1 (length var-names))))
        (perform (op call) (label cp-rtl-array-malloc))
        (restore ,(link-register))
        (restore ,(env-register))
        (perform (op store-array) (reg accum) (const 0) ,(env-register))
        (assign ,(env-register) (reg accum))))
     (preserving (scheme-preserved-registers)
                 (load-vars 1 exprs sub-env (empty-insn-seq))
                 (compile-body body sub-env)))))

