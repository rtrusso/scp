(define (scheme-codegen-lambda expr env target linkage)
  (define (build-varargs-environment index-of-last-arg)
    (let* ((copy-loop-token (make-label-token))
           (copy-begin-label (make-label 'varargs-copy-begin copy-loop-token))
           (copy-end-label (make-label 'varargs-copy-end copy-loop-token))
           (copy-link-label (make-label 'varargs-copy-link copy-loop-token)))
      (define (load-args iter)
        (if (< iter 0)
            '()
            (append `((assign (reg accum) (op load-array) (sys frame-pointer)
                              (const ,(+ 2 iter)))
                      (perform (op store-array)
                               ,(env-register)
                               (const ,(+ 1 iter))
                               (reg accum)))
                    (load-args (- iter 1)))))
      ;; This routine handles improper lambda argument lists, in which
      ;; excess arguments are accumulated in a list in the last argument.
      ;; We assume that the number of arguments is in the accum register, and that
      ;; it has not yet been overwritten.
      (insn-seq
       `(index ,(cadr (env-register)) ,(cadr (link-register)))
       '(accum index operand)
       `(
         ;; First, build the new environment object.
         ;;
         ;; Note on the save of reg index here, which might otherwise
         ;; seem unnecessary: It is assumed that index register (reg
         ;; index) already (or "still" depending on point-of-view)
         ;; contains the number of arguments to the closure.

         (save (reg index))
         (save ,(env-register))
         (save ,(link-register))
         (push (const ,(+ 1 1 index-of-last-arg)))
         (perform (op call) (label cp-rtl-array-malloc))
         (restore ,(link-register))
         (restore ,(env-register))
         (restore (reg index))
         (perform (op store-array) (reg accum) (const 0) ,(env-register))
         (assign ,(env-register) (reg accum))

         ;; At this point the new environment is built.  We must accumulate the
         ;; excess arguments in a list to store in the environment object.  We
         ;; start out with nil.  The list is built in reg accum.
         
         (save (reg index))
         (save ,(env-register))
         (save ,(link-register))
         (assign (reg accum) (label $scmliteral-nil))
         (label ,copy-begin-label)
         (branch-nonzero (label ,copy-end-label)
                         (result (op less-than-or-equal)
                                 (reg index)
                                 (const ,index-of-last-arg)))
         (save (reg index))
         (push (reg accum))
         (assign (reg operand) (op add) (reg index) (const 1))
         (assign (reg operand) (op bit-lshift) (reg operand)
                 (symconst shift-cells-per-word))
         (assign (reg index) (op add) (sys frame-pointer) (reg operand))
         (assign (reg accum) (op load-array) (reg index) (const 0))
         (push (reg accum))
         (assign ,(link-register) (label ,copy-link-label))
         (assign (reg accum) (label scmglue-cons))
         (assign (reg index) (const 2))
         (goto (reg accum))
         (label ,copy-link-label)
         (restore (reg index))
         (assign (reg index) (op sub) (reg index) (const 1))
         (goto (label ,copy-begin-label))
         (label ,copy-end-label)
         (restore ,(link-register))
         (restore ,(env-register))
         (restore (reg index))

         ;; Finally, we can initialize the varargs list in the spot in the
         ;; environment block reserved for the last arg.  The remaining
         ;; arguments (which at this point we have reduced the arg list to a known
         ;; fixed number) are loaded into the environment block and the routine
         ;; is complete.

         (perform (op store-array) ,(env-register) (const ,(+ 1 index-of-last-arg))
                  (reg accum))
         ,@(load-args (- index-of-last-arg 1))
         ))))
  (define (build-standard-environment number-args)
    (define (load-args iter number-args)
      (if (>= iter number-args)
          '()
          (append `((assign (reg accum) (op load-array) (sys frame-pointer)
                            (const ,(+ 1 iter)))
                    (perform (op store-array)
                             ,(env-register)
                             (const ,(+ 1 iter))
                             (reg accum)))
                  (load-args (+ iter 1) number-args))))
    (insn-seq
     `(,(cadr (env-register)) ,(cadr (link-register)))
     `(accum index operand ,(cadr (env-register)))
     `((save ,(env-register))
       (save ,(link-register))
       (push (const ,(+ 1 number-args)))
       (perform (op call) (const cp-rtl-array-malloc))
       (restore ,(link-register))
       (restore ,(env-register))
       (perform (op store-array) (reg accum) (const 0) ,(env-register))
       (assign ,(env-register) (reg accum))
       ,@(load-args 0 number-args))))
  ;; 1) Prologue - build an environment, assign argument registers, etc.
  ;; 2) Main function body
  ;; 3) continue linkage epilogue
  (let* ((lambda-label-token (make-label-token))
         (arg-check-ok-label (make-label 'arg-check-success lambda-label-token))
         (label (make-label-for-lambda-with-token lambda-label-token))
         (fixed-args? (list? (lambda-args expr)))
         (arg-names (lambda-arg-names expr))
         (body (lambda-code expr)))
    (define (allocate-lambda-at-runtime)
      (append-insn-seq
       (insn-seq
        `(,(cadr (link-register)) ,(cadr (env-register)))
        `(accum operand index)
        `((save ,(env-register))
          (save ,(link-register))
          (push (const 4))
          (perform (op call) (label cp-rtl-malloc))
          (restore ,(link-register))
          (restore ,(env-register))
          (perform (op store-array) (reg accum) (const 0) (const 1))
          (perform (op store-array) (reg accum) (const 1) (const ,(+ gc-array-mask 9)))
          (perform (op store-array) (reg accum) (const 2) ,(env-register))
          (perform (op store-array) (reg accum) (const 3) (label ,label))))
       (with-scheme-linkage
        (if (equal? target '(reg accum))
            (empty-insn-seq)
            (insn-seq
             '(accum)
             (list (if (eqv? (car target) 'reg)
                       (cadr target)
                       (error "Invalid target for lambda" target)))
             `((assign ,target (reg accum)))))
        linkage)))

    (let ((lambda-body-insn-seq
           (append-insn-seq
            (if fixed-args?
                ;; If this is a varargs function, make sure to push
                ;; the number of args on to the stack.  Otherwise,
                ;; nothing in particular is needed.
                (empty-insn-seq)
                (insn-seq '() '()
                          '((push (reg index)))))
            (insn-seq
             '()
             '()
             '((perform (op push-frame))
               (perform (op reserve-locals) (const 0))))
            (if fixed-args?
                (insn-seq '(index)
                          '(accum)
                          `((assign (reg accum) (op equal-to) (reg index) (const ,(length arg-names)))
                            (branch-nonzero (label ,arg-check-ok-label) (reg accum))
                            (assign (reg accum) (const ,(length arg-names)))
                            (push (reg accum))
                            (push (reg index))
                            (perform (op call) (label scheme-rtl-runtime-argument-count-error))
                            (label ,arg-check-ok-label)
                            ))
                (insn-seq '(index)
                          '(accum)
                          `(;(assign (reg accum) (op less-than-or-equal) (const ,(- (length arg-names) 1)) (reg index))
                                        ;(branch-nonzero (label ,arg-check-ok-label) (reg accum))
                            (branch-nonzero (label ,arg-check-ok-label)
                                            (result (op greater-than)
                                                    (reg index)
                                                    (const ,(- (length arg-names) 2))))
                            (assign (reg accum) (const ,(- (length arg-names) 1)))
                            (push (reg accum))
                            (push (reg index))
                            (perform (op call) (label scheme-rtl-runtime-argument-count-error))
                            (label ,arg-check-ok-label)
                            )))
            (if fixed-args?
                (build-standard-environment (length arg-names))
                (build-varargs-environment (- (length arg-names) 1)))
            (scheme-codegen `(begin ,@body)
                            (cons env arg-names)
                            '(reg accum)
                            (scheme-continue-linkage (if fixed-args?
                                                         (length arg-names)
                                                         'varargs))))))
      (scheme-codegen-emit-function! label lambda-body-insn-seq 0)
      (if (eqv? target 'immediate)
          (let ((closure-label (make-label-for-closure-with-token lambda-label-token)))
            (scheme-codegen-emit! (insn-seq '() '()
                                            `((global ,closure-label
                                                      (const 1)
                                                      (const ,(+ gc-array-mask 9))
                                                      (const 0)
                                                      (label ,label)))))
            `(label ,closure-label))
          (allocate-lambda-at-runtime)))))

