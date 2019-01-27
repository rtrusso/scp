(define (scheme-codegen-set! expr env target linkage)
  (define (get-lexical-address sym env up)
    (if (null? env)
        #f
        (let ((index (index-of sym eqv? (cdr env))))
          (if index
              (cons up index)
              (get-lexical-address sym (car env) (+ up 1))))))
  (define (codegen-lexical-lookup up over current-env)
    ;; (display ";; codegen-lexical-lookup ")
    ;; (display up)
    ;; (display " ")
    ;; (display over)
    ;; (newline)
    (if (zero? up)
        (append-insn-seq
         (insn-seq (list (cadr current-env))
                   '()
                   `((perform (op store-array) ,current-env (const ,(+ over 1)) (reg operand))
                     ))
         (if (eqv? target 'null)
             (empty-insn-seq)
             (insn-seq '()
                       (target-modifies target)
                       `((assign ,target (label $scmliteral-unspecified))))))
        (append-insn-seq
         (insn-seq (list (cadr current-env))
                   '(accum)
                   `((assign (reg accum) (op load-array) ,current-env (const 0))))
         (codegen-lexical-lookup (- up 1) over '(reg accum)))))
  (let ((addr (get-lexical-address (set!-symbol expr) env 0)))
    (preserving
     (scheme-preserved-registers)
     (scheme-codegen (set!-code expr) env '(reg operand) (next-linkage))
     (with-scheme-linkage
      (if addr
          (codegen-lexical-lookup (car addr) (cdr addr) (env-register))
          (begin
            (if (not (member (set!-symbol expr) *top-level-referenced-symbols*))
               (set! *top-level-referenced-symbols*
                     (cons (set!-symbol expr)
                           *top-level-referenced-symbols*)))
            (append-insn-seq
             (insn-seq
             '(operand)
             '()
             `((perform (op store-array)
                        (label ,(make-label-for-global (set!-symbol expr)))
                        (const 0)
                        (reg operand))))
             (if (eqv? target 'null)
                 (empty-insn-seq)
                 (insn-seq '()
                           (target-modifies target)
                           `((assign ,target (label $scmliteral-unspecified))))))
            ))
      linkage))))

