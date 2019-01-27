;; Calling convention
;;
;;  - Arguments passed in reverse order, on the stack as usual
;;  - The number of arguments is passed in (reg index)
;;  - The return linkage is passed in (link-register)
;;  - for varargs functions, the number of args (initially in (reg index)),
;;    is stored on the stack just after arg 0 and just before the frame pointer
;;


;; Lambda Epilogues
;;
;; The following routines generate code for lambda epilogues.  These
;; come in four varieties according to the following matrix:
;;
;; [ fixed vs. varargs ] X [ tail call vs. result ]
;;

(define (scheme-codegen-continue-with-accum-fixed-args n-args)
  (insn-seq `(accum ,(cadr (link-register)))
            '()
            `((perform (op pop-frame))
              (assign (sys stack-pointer) (op add-pointer) (sys stack-pointer)
                      (const ,n-args))
              (goto ,(link-register)))))

(define (scheme-codegen-continue-with-accum-varargs)
  (insn-seq `(,(cadr (link-register)))
            '(index)
            `((perform (op pop-frame))
              ;; We load the number of args up into (reg accum) using a pop
              ;; instruction.
              (pop (reg index))
              (assign (reg index) (op bit-lshift) (reg index)
                      (symconst shift-cells-per-word))
              (assign (sys stack-pointer)
		      (op add) (sys stack-pointer) (reg index))
              (goto ,(link-register)))))

;; Lambda Epilogues - Tail Call Versions
;;
;; The tail-call routines have these important propties:
;;   1) These routines will take care of setting (reg index) to the number of
;;      arguments passed to the target of the tail call
;;   2) The address of the routine to be chained must be in (reg operand), the
;;      environment is already initialized in (env-register), the chained link
;;      is in (link-register)
;;   3) The accumulator (reg accum) is "fair game" for the instructions produced
;;      by these routines and it can/will be modified.
;;

(define (scheme-codegen-continue-with-tail-call-fixed-args n-args-in n-args-out)
  (define (repeat-copy-arg times)
    (if (zero? times)
        (empty-insn-seq)
        (append-insn-seq
         (insn-seq '(accum)
                   '(accum index)
                   `((assign (reg index) (op load-array)
                             (reg accum) (const 0))
                     (push (reg index))
                     ,@(if (> times 1)
                           '((assign (reg accum) (op sub-pointer)
                                     (reg accum) (const 1)))
                           '())))
         (repeat-copy-arg (- times 1)))))
  (append-insn-seq
   (insn-seq '() ;USED TO BE: `(,(cadr (link-register)))
             '(accum)
             `((assign (reg accum) (op add-pointer) (sys stack-pointer)
                       (const ,(- n-args-out 1)))
               (perform (op pop-frame))
               (assign (sys stack-pointer) (op add-pointer) (sys stack-pointer)
                       (const ,n-args-in))))
   (repeat-copy-arg n-args-out)
   (insn-seq `(,(cadr (link-register)) ,(cadr (env-register)))
             '() ;; all registers will be modified, but since this is a tail call it doesn't matter
             `((assign (reg index) (const ,n-args-out))
               (goto (reg operand))))))

