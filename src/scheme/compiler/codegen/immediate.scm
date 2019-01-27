(define (scheme-codegen-immediate expr target linkage)
  (let
      ((label (cond ((boolean? expr)
                     (if expr '$scmliteral-true '$scmliteral-false))
                    ((null? expr)
                     '$scmliteral-nil)
                    (else
                     (make-label-for-literal))))
       (data (cond ((eqv? 'null target)
                    (empty-insn-seq))
                   ((integer? expr)
                    (insn-seq '() '()
                              `((data (const ,expr))
                                (data (const 1)))))
                   ((or (boolean? expr)
                        (null? expr))
                    (empty-insn-seq))
                   ((char? expr)
                    (insn-seq '() '()
                              `((data (const ,(char->integer expr)))
                                (data (const 3)))))
                   ((pair? expr)
                    (insn-seq
                     '() '()
                     `((data (const 2))
                       (data (const ,(+ gc-array-mask 4)))
                       (data ,(scheme-codegen-immediate (car expr)
                                                        'immediate
                                                        (next-linkage)))
                       (data ,(scheme-codegen-immediate (cdr expr)
                                                        'immediate
                                                        (next-linkage))))))
                   ((vector? expr)
                    (insn-seq
                     '() '()
                     `((data (const ,(vector-length expr)))
                       (data (const ,(+ gc-array-mask 5)))
                       ,@(map
                          (lambda (x)
                            `(data ,(scheme-codegen-immediate
                                     x
                                     'immediate
                                     (next-linkage))))
                          (vector->list expr)))))
                   ((string? expr)
                    (insn-seq '() '()
                              `((data (const ,(string-length expr)))
                                (data (const 6))
                                ,@(map (lambda (x)
                                         `(data (const ,(char->integer x))))
                                       (string->list expr))
                                (data (const 0)))))
                   ((symbol? expr)
                    (insn-seq '() '()
                              `((data
                                 (const ,(string-length
                                          (symbol->string expr))))
                                (data (const 7))
                                ,@(map (lambda (x)
                                         `(data (const ,(char->integer x))))
                                       (string->list (symbol->string expr))))))
                   (else
                    (error "Invalid immediate expression" expr)))))
    (if (and (not (eqv? target 'null))
             (not (null? (insn-seq:insns data))))
        (scheme-codegen-emit!
         (insn-seq '() '()
                   `((global ,label
                             ,@(map cadr (insn-seq:insns data)))))))
    (cond ((eqv? 'immediate target)
           `(label ,label))
          ((eqv? 'null target)
           (empty-insn-seq))
          (else
           (with-scheme-linkage
            (insn-seq '() (target-modifies target)
                      `((assign ,target (label ,label))))
            linkage)))))

