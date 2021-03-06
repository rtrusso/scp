(define (scheme-codegen-application expr env target linkage)
  (define (codegen-args rest)
    (if (null? rest)
        (empty-insn-seq)
        (preserving
         (scheme-preserved-registers)
         (append-insn-seq
          (scheme-codegen (car rest) env '(reg accum) (next-linkage))
          (let ((label (make-label 'scheme-arg-check-link (make-label-token))))
            (insn-seq '(accum) '()
                      `((branch-nonzero (label ,label) (reg accum))
                        (push (const ,(current-milliseconds)))
                        (perform (op break))
                        (label ,label)
                        (push (reg accum)))))
          )
         (codegen-args (cdr rest)))))

  (define (codegen-routine routine-expr)
    (scheme-codegen routine-expr env '(reg operand) (next-linkage)))

  (if (null? expr)
      (error "application of a null expr" expr))
  (preserving
   `(,(cadr (link-register)))
   (append-insn-seq
    (preserving
     (scheme-preserved-registers)
     (codegen-args (reverse (cdr expr)))
     (codegen-routine (car expr)))
    (let ((label (make-label 'function-check (make-label-token))))
      (insn-seq '(operand)
                `(operand index ,(cadr (env-register)))
                `((assign ,(env-register) (op load-array) (reg operand) (const 1))
                  (branch-nonzero (label ,label) (result (op equal-to) ,(env-register) (const -2147483639)))
                  (perform (op break))
                  (label ,label)
                  (assign ,(env-register) (op load-array) (reg operand) (const 2))
                  (assign (reg operand) (op load-array) (reg operand) (const 3)))))
    )
   (cond ((linkage-next? linkage)
          (let ((label (make-label 'scheme-apply-link (make-label-token))))
            (append-insn-seq
             (insn-seq `(operand ,(cadr (env-register)))
                       `(accum operand index
                               ,(cadr (env-register)) ,(cadr (link-register)))
                       `((assign (reg link) (label ,label))
                         (assign (reg index) (const ,(length (cdr expr))))
                         (goto (reg operand))
                         (label ,label)))
             (cond ((or (equal? target '(reg accum))
                        (eqv? 'null target))
                    (empty-insn-seq))
                   ((and (pair? target) (eqv? 'reg (car target)))
                    (insn-seq '(accum) (target-modifies target)
                              `((assign ,target (reg accum)))))
                   (else
                    (error "unrecognized target for application next linkage" target))))))
         ((linkage-goto? linkage)
          (if (not (or (equal? target '(reg accum))
                       (equal? target 'null)))
              (error "unexpected target with application goto linkage" target))
          (insn-seq `(operand ,(cadr (env-register)))
                    `(accum operand index
                            ,(cadr (env-register)) ,(cadr (link-register)))
                    `((assign (reg link) (label ,(goto-linkage-target linkage)))
                      (assign (reg index) (const ,(length (cdr expr))))
                      (goto (reg operand)))))
         ((scheme-continue-linkage? linkage)
          (if (not (equal? target '(reg accum)))
              (error "unexpected target with application continue linkage" target))
          (let ((link (scheme-continue-linkage-arg-count linkage)))
            (if (number? link)
                (scheme-codegen-continue-with-tail-call-fixed-args
                                                           link (length (cdr expr)))
                (scheme-codegen-continue-with-tail-call-varargs
                                                             (length (cdr expr))))))
         (else
          (error "Unrecognized linkage for application" linkage)))))

