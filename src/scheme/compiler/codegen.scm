;; (define *scheme-codegen-depth* 0)
;; (define (scheme-codegen expr env target linkage)
;;   (display (make-string *scheme-codegen-depth* #\space))
;;   (display ";; scheme-codegen ")
;;   (display expr)
;;   (newline)
;;   (set! *scheme-codegen-depth* (+ 1 *scheme-codegen-depth*))
;;   (let ((x (scheme-codegen-internal expr env target linkage)))
;;     (set! *scheme-codegen-depth* (- *scheme-codegen-depth* 1))
;;     (display (make-string *scheme-codegen-depth* #\space))
;;     (display ";; done")
;;     (newline)
;;     x))

(define (scheme-codegen expr env target linkage)
  (cond
   ((null? expr)
    (scheme-codegen-immediate expr target linkage))
   ;; expression: literals
   ((immediate? expr)
    (scheme-codegen-immediate expr target linkage))
   ;; special-form: quote
   ((quoted-immediate? expr)
    (scheme-codegen-immediate (quoted-expression expr) target linkage))
   ;; special-form: define
   ((and (pair? expr)
         (eqv? 'define (car expr))
         (pair? (cdr expr))
         (symbol? (cadr expr))
         (pair? (cddr expr))
         (null? (cdddr expr)))
    (let ((global-label (make-label-for-global (cadr expr)))
          (value-expr (caddr expr)))
      (define (define-at-runtime)
        ;; Emit code to reserve space for the definition.
        (scheme-codegen-emit! (insn-seq '() '()
                                        `((global ,global-label
                                                  (const 0)))))
        ;; Return an instruction sequence that will perform the side
        ;; effect of setting the value of the definition.  Notice that
        ;; we ignore target, since the resulting value of a definition
        ;; is unspecified.  In the future we may want to set the
        ;; target to some known value (#<unspecified> as seen in
        ;; mzscheme).
        (with-scheme-linkage
         (preserving (scheme-preserved-registers)
                     (scheme-codegen value-expr env '(reg accum) (next-linkage))
                     (insn-seq '(accum) '()
                               `((perform (op store-array)
                                          (label ,global-label)
                                          (const 0)
                                          (reg accum)))))
         linkage))
      (define (define-at-compile-time)
        (let ((raw-data
               (cond ((immediate? value-expr)
                      (scheme-codegen-immediate value-expr
                                                'immediate
                                                (next-linkage)))
                     ((quoted-immediate? value-expr)
                      (scheme-codegen-immediate (quoted-expression value-expr)
                                                'immediate
                                                (next-linkage)))
                     ((and (pair? value-expr)
                           (eqv? 'lambda (car value-expr)))
                      (scheme-codegen-lambda value-expr
                                             env
                                             'immediate
                                             (next-linkage)))
                     (else
                      (error "invalid expression for define-at-compile-time")))
               ))
          ;; Emit code to reserve space for the definition.
          (scheme-codegen-emit! (insn-seq '() '()
                                          `((global ,global-label
                                                    ,raw-data))))
          (empty-insn-seq)))
      (let ((result (cond ((or (immediate? value-expr)
                               (quoted-immediate? value-expr)
                               (and (pair? value-expr)
                                    (eqv? 'lambda (car value-expr))))
                           (define-at-compile-time))
                          (else
                           (define-at-runtime)))))
        result)))
   ;; special-form: if
   ((and (pair? expr)
         (eqv? 'if (car expr))
         (list? expr)
         (or (= 3 (length expr))
             (= 4 (length expr))))
    (let* ((predicate (list-ref expr 1))
           (consequent (list-ref expr 2))
           (antecedent (if (= 2 (length expr))
                           '(quote ())
                           (list-ref expr 3)))
           (token (make-label-token))
           (else-label (make-label 'if-else token))
           (end-label (and (linkage-next? linkage)
                           (make-label 'if-end token)))
           (end-insn (if (linkage-next? linkage)
                         (label-insn-seq end-label)
                         (empty-insn-seq)))
           (t-linkage (if (linkage-next? linkage)
                          (link-goto end-label)
                          linkage))
           (p-code (scheme-codegen predicate env '(reg accum) (next-linkage)))
           (t-code (scheme-codegen consequent env target t-linkage))
           (f-code (if (= 2 (length expr))
                       (if (eqv? target 'null)
                           (empty-insn-seq)
                           (insn-seq '()
                                     (target-modifies target)
                                     `((assign ,target (label $scmliteral-unspecified)))))
                       (scheme-codegen antecedent env target linkage))
                   )
           )
      (preserving (scheme-preserved-registers)
                  p-code
                  (append-insn-seq
                   (insn-seq '(accum) '(operand)
                             `((assign (reg operand) (op equal-to) (reg accum) (label $scmliteral-false))
                               (branch-nonzero (label ,else-label) (reg operand))
                               ))
                   (parallel-insn-seq
                    t-code
                    (append-insn-seq (label-insn-seq else-label)
                                     f-code))
                   end-insn))))
   ;; special-form: begin
   ((and (pair? expr)
         (eqv? 'begin (car expr))
         (list? (cdr expr)))
    (if (null? (cdr expr))
        ;; This is a null 'begin' expression, we handle it as a special case.
        (scheme-codegen-immediate '() target linkage)
        (let loop ((exprs (cdr expr))
                   (code (empty-insn-seq)))
          (cond ((null? (cdr exprs))
                 ;; This is the last code item in the begin expression.
                 (preserving (scheme-preserved-registers)
                             code
                             (scheme-codegen (car exprs) env target linkage)))
                (else
                 ;; This expression is followed by others in the begin
                 ;; expression.  It is evaluated only for side effects.
                 (loop (cdr exprs)
                       (preserving (scheme-preserved-registers)
                                   code
                                   (scheme-codegen (car exprs) env 'null
                                                   (next-linkage)))))))))
   ;; "special-form": letrec
   ((and (pair? expr)
         (eqv? 'letrec (car expr)))
    (scheme-codegen-letrec expr env target linkage))
   ;; special-form: lambda
   ((and (pair? expr)
         (eqv? 'lambda (car expr)))
    (scheme-codegen-lambda expr env target linkage))
   ;; special-form: set!
   ((and (pair? expr)
         (eqv? 'set! (car expr)))
    (scheme-codegen-set! expr env target linkage))
   ;; expression: variable-reference
   ((symbol? expr)
    (scheme-codegen-variable-reference expr env target linkage))
   ;; special-form: need
   ((and *conspiracy-mode*
         (pair? expr)
         (eqv? 'need (car expr)))
    (compile-need! (string-append (symbol->string (cadr expr))
                                  ".scm"))
    (empty-insn-seq))
   ;; special-form: rfilact
   ((and *conspiracy-mode*
         (pair? expr)
         (eqv? 'rfilact (car expr)))
    (error "unimplemented - rfilact"))
   ;; expression: application
   (else
    (scheme-codegen-application expr env target linkage))))

