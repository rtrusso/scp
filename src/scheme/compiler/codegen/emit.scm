(define (scheme-codegen-environment)
  '())

(define (scheme-codegen-emit! insn-seq)
  (for-each (lambda (datum)
              (set! *scheme-codegen-emit-delayed*
                    (cons datum
                          *scheme-codegen-emit-delayed*)))
            (insn-seq:insns insn-seq)))

(define (scheme-codegen-emit-function! name body-insn-seq locals)
  (set! *scheme-codegen-emit-delayed-functions*
        (cons `(function (name ,name)
                         (locals ,locals)
                         (body ,@(insn-seq:insns body-insn-seq)))
              *scheme-codegen-emit-delayed-functions*)))

(define (scheme-codegen-emit-flush!)
  (pretty-print `(extern $scmliteral-false) *output*)
  (newline *output*)
  (pretty-print `(extern $scmliteral-true) *output*)
  (newline *output*)
  (pretty-print `(extern $scmliteral-nil) *output*)
  (newline *output*)
  (pretty-print `(extern $scmliteral-unspecified) *output*)
  (newline *output*)
  (pretty-print `(extern scmglue-cons) *output*)
  (newline *output*)
  (pretty-print `(extern cp-rtl-array-malloc) *output*)
  (newline *output*)
  (pretty-print `(extern cp-rtl-malloc) *output*)
  (newline *output*)
  (pretty-print `(extern scheme-rtl-runtime-argument-count-error) *output*)
  (newline *output*)
  (pretty-print `(extern cp-rtl-add-global-root-range) *output*)
  (newline *output*)
  (if *emit-global-marks*
      (begin
        (pretty-print `(export mm-rtl-heap-begin-mark-global-vars) *output*)
        (newline *output*)
        (pretty-print `(export mm-rtl-heap-end-mark-global-vars) *output*)
        (newline *output*)))
  (for-each (lambda (symbol)
              (if (not (member symbol *top-level-defined-symbols*))
                  (begin
                    (pretty-print `(extern ,(make-label-for-global symbol))
                                  *output*)
                    (newline *output*))))
            (reverse *top-level-referenced-symbols*))
  (for-each (lambda (symbol)
              (pretty-print `(export ,(make-label-for-global symbol))
                            *output*)
              (newline *output*))
            (reverse *top-level-defined-symbols*))
  (if *emit-global-marks*
      (begin
        (pretty-print `(global mm-rtl-heap-begin-mark-global-vars (const 0))
                      *output*)
        (newline *output*)))
  (pretty-print '(global begin-mark-global-vars-range (const 0)) *output*)
  (newline *output*)
  (pretty-print '(global end-mark-global-vars-range-pointer (label end-mark-global-vars-range)) *output*)
  (newline *output*)
  (for-each (lambda (x)
              (pretty-print x *output*)
              (newline *output*))
            (reverse *scheme-codegen-emit-delayed*))
  (pretty-print '(global end-mark-global-vars-range (const 0)) *output*)
  (newline *output*)
  (if *emit-global-marks*
      (begin
        (pretty-print `(global mm-rtl-heap-end-mark-global-vars (const 0))
                      *output*)
        (newline *output*)))
  (for-each (lambda (x)
              (pretty-print x *output*)
              (newline *output*))
            (reverse *scheme-codegen-emit-delayed-functions*))
  (newline *output*)
  (reset-compiler-context!)
  )

