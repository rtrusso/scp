;; meta-eval.scm
;;
;; An implementation of eval.

(need scheme/syntax/expander)
(need scheme/transforms/internal-defines)
(need scheme/base-syntax)
(need util/rfilact)

(define (meta-eval expression environment)
  (environment expression))

(define (meta-eval-environment)
  (let ((macro-environment (top-level-env))
        (unspec (let ((foo 0)) (set! foo 0)))
        (file-scope '())
        (tentative-file-scope '()))

    (define (define-top-level-symbol! symbol value)
      (let ((symbol-entry (assoc symbol file-scope)))
        (if symbol-entry
            (begin (set-cdr! symbol-entry value)
                   symbol-entry)
            (let ((new-symbol-entry (cons symbol value)))
              (set! file-scope (cons new-symbol-entry file-scope))
              new-symbol-entry))))

    (define (lookup-top-level-symbol symbol)
      (let ((symbol-entry (assoc symbol file-scope)))
        (if symbol-entry
            symbol-entry
            (error "Undefined top-level symbol" symbol))))

    ;;
    ;; The lookup-lexical-symbol routine maps symbol names through the
    ;; environment to either top-level environment symbol-entries (a
    ;; pair consisting of a symbol and its current value) or lexical
    ;; environment symbol-entries [expressed as an <up, over>
    ;; coordinate, in a pair as (cons symbol (cons up over))].
    ;;
    (define (lookup-lexical-symbol symbol environment)
      (if (null? environment)
          (lookup-top-level-symbol symbol)
          (let ((current-symbol-entry (assoc symbol (lexical-environment-symbols environment))))
            (if current-symbol-entry
                (cons 0 (cdr current-symbol-entry))
                (let ((upper-symbol-entry (lookup-lexical-symbol symbol (lexical-environment-parent environment))))
                  (if (symbol? (car upper-symbol-entry))
                      upper-symbol-entry
                      (cons (+ 1 (car upper-symbol-entry))
                            (cdr upper-symbol-entry))))))))

    (define (lexical-environment-parent lexical-environment)
      (car lexical-environment))

    (define (lexical-environment-symbols lexical-environment)
      (cdr lexical-environment))

    (define (expand-expression expression)
      (rewrite-defines (scheme-expand-expression expression
                                                 macro-environment)))

    (define (expression-evaluator expression lexical-environment)
      ;;
      ;; This code is the heart of the interpreter, translating Scheme
      ;; s-exps to lambda expressions.
      ;;
;      (display "expression evaluator: ")
;      (write expression)
;      (newline)
;      (display "    lexical environment: ")
;      (write lexical-environment)
;      (newline)
      (cond

       ;; immediate expressions
       ((or (test-immediate? expression)
            (null? expression)
            (procedure? expression))
        (lambda (runtime-environment) expression))

       ;; quoted expressions
       ((test-quoted? expression)
        (if (and (list? expression)
                 (= 2 (length expression)))
            (let ((value (quoted-value expression)))
              (lambda (runtime-environment) value))
            (error "Invalid quoted value syntax" expression)))

       ;;
       ;; Variable references
       ;;
       ;; These may be global references, in which case the variable
       ;; is resolved to a storage address at runtime, or they are
       ;; lexically-scoped references, in which case they are resolved
       ;; to a storage address at runtime.
       ;;
       ((test-ref? expression)
        (let ((symbol-entry (lookup-lexical-symbol expression
                                                   lexical-environment)))

;          (display "    symbol reference: ")
;          (write expression)
;          (display " ")
;          (write symbol-entry)
;          (newline)

          (if (symbol? (car symbol-entry))

              ;;
              ;; Global variable - the easy case
              ;;
              (lambda (runtime-environment)
                (cdr symbol-entry))

              ;;
              ;; Lexically-scoped variable - build a lambda expression
              ;; which will look up the value in the lexical
              ;; environment at runtime.
              ;;
              (let ((up (car symbol-entry))
                    (over (cdr symbol-entry)))
                (let loop ((evaluator (lambda (runtime-environment)
                                        (vector-ref runtime-environment (+ 1 over))))
                           (up up))
                  (if (zero? up)
                      evaluator
                      (loop (lambda (runtime-environment)
;                              (display "    ref 1up: ")
;                              (write runtime-environment)
;                              (newline)
                              (evaluator (vector-ref runtime-environment 0)))
                            (- up 1)))))
              )))

       ;;
       ;; Variable modification - As with variable reference, can be
       ;; global or lexically-scoped variables.  The same evaluation
       ;; strategy is used.
       ;;
       ((test-set!? expression)
        (let ((symbol-entry (lookup-lexical-symbol (set!-symbol expression)
                                                   lexical-environment))
              (value-evaluator (expression-evaluator (set!-code expression)
                                                     lexical-environment)))

;          (display "    symbol modification: ")
;          (write expression)
;          (display " ")
;          (write symbol-entry)
;          (newline)

          (if (symbol? (car symbol-entry))

              ;;
              ;; Global variable - the easy case
              ;;
              (lambda (runtime-environment)
                (set-cdr! symbol-entry (value-evaluator runtime-environment)))

              ;;
              ;; Lexically-scoped variable - build a lambda expression
              ;; which will look up the value in the lexical
              ;; environment at runtime.
              ;;
              (let ((up (car symbol-entry))
                    (over (cdr symbol-entry)))
                (let loop ((evaluator (lambda (runtime-environment value)
                                        (vector-set! runtime-environment
                                                     (+ 1 over)
                                                     value)))
                           (up up))
                  (if (zero? up)
                      (lambda (runtime-environment)
                        (evaluator runtime-environment
                                   (value-evaluator runtime-environment)))
                      (loop (lambda (runtime-environment value)
;                              (display "    set 1up: ")
;                              (write runtime-environment)
;                              (newline)
                              (evaluator (vector-ref runtime-environment 0)
                                         value))
                            (- up 1)))))
              )))

       ;;
       ;; Conditional expression
       ;;
       ((test-if? expression)
        (let ((predicate (expression-evaluator (if-predicate expression)
                                               lexical-environment))
              (consequent (expression-evaluator (if-consequent expression)
                                                lexical-environment))
              (antecedent (expression-evaluator (if-antecedent expression)
                                                lexical-environment)))
          (lambda (runtime-environment)
            (let ((result (predicate runtime-environment)))
              (if result
                  (consequent runtime-environment)
                  (antecedent runtime-environment))))))

       ;;
       ;; Begin expression
       ;;
       ((test-begin? expression)
        (if (null? (begin-sequence expression))
            (error "Invalid empty begin expression" expression)
            (let loop ((evaluator (expression-evaluator (car (begin-sequence expression))
                                                        lexical-environment))
                       (sequence (cdr (begin-sequence expression))))
              (if (null? sequence)
                  evaluator
                  (loop (let ((next-evaluator (expression-evaluator (car sequence)
                                                                    lexical-environment)))
                          (lambda (runtime-environment)
                            (evaluator runtime-environment)
                            (next-evaluator runtime-environment)))
                        (cdr sequence))))))

       ;;
       ;; Define statement - handles top-level environment definitions
       ;; using the define syntax.  This only handles top-level
       ;; environment definitions because internal defines are
       ;; transformed into letrec by the expander.
       ;;
       ((test-define? expression)
        (let ((symbol-entry
               (define-top-level-symbol! (define-symbol expression) unspec))
              (code (expression-evaluator (define-code expression)
                                          lexical-environment)))
          (lambda (runtime-environment)
            (set-cdr! symbol-entry (code runtime-environment)))))

       ;;
       ;; letrec expressions -- converted into a lambda expression
       ;; that is equivalent disregarding the effect of continuations.
       ;;
       ((test-letrec? expression)
;        (display "    letrec->lambda: ")
;        (write (letrec->lambda-general expression '--unspecified--))
;        (newline)
        (expression-evaluator (letrec->lambda-general expression '((lambda (x) (set! x 0)) 0))
                              lexical-environment))

       ;;
       ;; lambda expressions -- may be 'improper' lambda expressions
       ;; that genberate functions taking N or more arguments, or
       ;; 'proper' lambda expressions generating functions that take
       ;; precisely M arguments.
       ;;
       ((test-lambda? expression)
        (let ((syntax-parameters (lambda-args expression))
              (parameters (improper-lambda-args expression))
              (parameter-number-counter 0))
          (define (build-lexical-environment)
            (cons lexical-environment
                  (map (lambda (parameter)
                         (let ((parameter-number parameter-number-counter))
                           (set! parameter-number-counter (+ 1 parameter-number-counter))
                           (cons parameter parameter-number)))
                       parameters)))
          (let ((body-evaluator
                 (expression-evaluator (cons 'begin
                                             (lambda-code expression))
                                       (build-lexical-environment)))
                (parameter-count (length parameters)))

            (if (list? syntax-parameters)

                ;;
                ;; Simple, proper lambda case
                ;;
                (lambda (captured-runtime-environment)
;                  (display "    invoke-lambda: ")
;                  (write expression)
;                  (display "     environment: ")
;                  (write captured-runtime-environment)
;                  (newline)
                  (lambda runtime-args
                    (if (= (length runtime-args) parameter-count)
                        (let ((new-runtime-environment (make-vector (+ 1 parameter-count))))
;                          (display "    new runtime env: ")
;                          (write new-runtime-environment)
;                          (newline)
                          (let loop ((counter 0)
                                     (args runtime-args))
                            (if (= counter parameter-count)
                                (begin
                                  (vector-set! new-runtime-environment
                                               0
                                               captured-runtime-environment)
;                                  (display "    fixed runtime env: ")
;                                  (write new-runtime-environment)
;                                  (newline)
                                  (body-evaluator new-runtime-environment))
                                (begin
                                  (vector-set! new-runtime-environment
                                               (+ counter 1)
                                               (car args))
                                  (loop (+ counter 1) (cdr args))))))
                        (error "Invalid arguments" runtime-args parameter-count))))

                ;;
                ;; Improper lambda case
                ;;
                (lambda (captured-runtime-environment)
                  (lambda runtime-args
                    (if (>= (length runtime-args) (- parameter-count 1))
                        (let ((new-runtime-environment (make-vector (+ 1 parameter-count))))
                          (let loop ((counter 0)
                                     (args runtime-args))
                            (if (= (+ 1 counter) parameter-count)
                                (begin
                                  (vector-set! new-runtime-environment
                                               (+ 1 counter)
                                               args)
                                  (vector-set! new-runtime-environment
                                               0
                                               captured-runtime-environment)
                                  (body-evaluator new-runtime-environment))
                                (begin (vector-set! new-runtime-environment
                                                    (+ counter 1)
                                                    (car args))
                                       (loop (+ counter 1)
                                             (cdr args))))))
                        (error "Invalid arguments"
                               runtime-args
                               (- parameter-count 1)))))
                ))))

       ;;
       ;; apply
       ;;
       ((list? expression)
        (let ((operation-evaluator (expression-evaluator (car expression)
                                                         lexical-environment))
              (operand-evaluators (map (lambda (operand)
                                         (expression-evaluator operand
                                                               lexical-environment))
                                       (cdr expression))))
          (lambda (runtime-environment)
            (let ((operation (operation-evaluator runtime-environment))
                  (args (map (lambda (operand)
                               (operand runtime-environment))
                             operand-evaluators)))
              (apply operation args)))))

       (else (error "Unrecognized expression" expression))
       ))

    (lambda (expression)
      (let ((evaluator (expression-evaluator (expand-expression expression)
                                             '())))
        (evaluator (make-vector 0))))))

(define (meta-eval-r5rs-environment)
  (let ((environment (meta-eval-environment)))

    ;;
    ;; Load R5RS syntax
    ;;
;    (display ";; loading r5rs syntax")
;    (newline)
    (for-each (lambda (expression) (meta-eval expression environment))
              (read-file-into-list-at-compile-time "rtl/r5rs-syntax.scm"))

    ;;
    ;; Load R5RS procedures
    ;;
;    (display ";; loading r5rs procedures")
;    (newline)
    (for-each (lambda (entry)
                (meta-eval `(define ,(car entry) (quote ,(cdr entry)))
                           environment))
              `((vector-ref . ,vector-ref)
                (make-vector . ,make-vector)
                (eqv? . ,eqv?)
                (eq? . ,eq?)
                (equal? . ,equal?)
                (number->string . ,number->string)
                (string->number . ,string->number)
                (not . ,not)
                (boolean? . ,boolean?)
                (pair? . ,pair?)
                (cons . ,cons)

                (car . ,car)
                (cdr . ,cdr)

                (caar . ,caar)
                (cadr . ,cadr)
                (cdar . ,cdar)
                (cddr . ,cddr)

                (caaar . ,caaar)
                (caadr . ,caadr)
                (cadar . ,cadar)
                (caddr . ,caddr)
                (cdaar . ,cdaar)
                (cdadr . ,cdadr)
                (cddar . ,cddar)
                (cdddr . ,cdddr)

                (caaaar . ,caaaar)
                (caaadr . ,caaadr)
                (caadar . ,caadar)
                (caaddr . ,caaddr)
                (cadaar . ,cadaar)
                (cadadr . ,cadadr)
                (caddar . ,caddar)
                (cadddr . ,cadddr)
                (cdaaar . ,cdaaar)
                (cdaadr . ,cdaadr)
                (cdadar . ,cdadar)
                (cdaddr . ,cdaddr)
                (cddaar . ,cddaar)
                (cddadr . ,cddadr)
                (cdddar . ,cdddar)
                (cddddr . ,cddddr)

                (set-car! . ,set-car!)
                (set-cdr! . ,set-cdr!)
                (caar . ,caar)
                (cadr . ,cadr)
                (cdddar . ,cdddar)
                (cddddr . ,cddddr)
                (null? . ,null?)
                (list? . ,list?)
                (list . ,list)
                (length . ,length)
                (append . ,append)
                (reverse . ,reverse)
                (list-tail . ,list-tail)
                (list-ref . ,list-ref)
                (memq . ,memq)
                (memv . ,memv)
                (member . ,member)
                (assq . ,assq)
                (assv . ,assv)
                (assoc . ,assoc)
                (symbol? . ,symbol?)
                (symbol->string . ,symbol->string)
                (string->symbol . ,string->symbol)
                (char? . ,char?)
                (char=? . ,char=?)
                (char<? . ,char<?)
                (char>? . ,char>?)
                (char<=? . ,char<=?)
                (char>=? . ,char>=?)
                (char-ci=? . ,char-ci=?)
                (char-ci<? . ,char-ci<?)
                (char-ci>? . ,char-ci>?)
                (char-ci<=? . ,char-ci<=?)
                (char-ci>=? . ,char-ci>=?)
                (char-alphabetic? . ,char-alphabetic?)
                (char-numeric? . ,char-numeric?)
                (char-whitespace? . ,char-whitespace?)
                (char-upper-case? . ,char-upper-case?)
                (char-lower-case? . ,char-lower-case?)
                (char->integer . ,char->integer)
                (integer->char . ,integer->char)
                (char-upcase . ,char-upcase)
                (char-downcase . ,char-downcase)
                (string? . ,string?)
                (make-string . ,make-string)
                (string . ,string)
                (string-length . ,string-length)
                (string-ref . ,string-ref)
                (string-set! . ,string-set!)
                (string=? . ,string=?)
                (string-ci=? . ,string-ci=?)
                (string<? . ,string<?)
                (string>? . ,string>?)
                (string<=? . ,string<=?)
                (string>=? . ,string>=?)
                (string-ci<? . ,string-ci<?)
                (string-ci>? . ,string-ci>?)
                (string-ci<=? . ,string-ci<=?)
                (string-ci>=? . ,string-ci>=?)
                (substring . ,substring)
                (string-append . ,string-append)
                (string->list . ,string->list)
                (list->string . ,list->string)
                (string-copy . ,string-copy)
                (string-fill! . ,string-fill!)
                (vector? . ,vector?)
                (make-vector . ,make-vector)
                (vector . ,vector)
                (vector-length . ,vector-length)
                (vector-ref . ,vector-ref)
                (vector-set! . ,vector-set!)
                (vector->list . ,vector->list)
                (list->vector . ,list->vector)
                (vector-fill! . ,vector-fill!)
                (procedure? . ,procedure?)
                (apply . ,apply)
                (map . ,map)
                (for-each . ,for-each)
                (call-with-input-file . ,call-with-input-file)
                (call-with-output-file . ,call-with-output-file)
                (input-port? . ,input-port?)
                (output-port? . ,output-port?)
                (current-input-port . ,current-input-port)
                (current-output-port . ,current-output-port)
                (open-input-file . ,open-input-file)
                (open-output-file . ,open-output-file)
                (close-input-port . ,close-input-port)
                (close-output-port . ,close-output-port)
                (read . ,read)
                (read-char . ,read-char)
                (peek-char . ,peek-char)
                (eof-object? . ,eof-object?)
                (char-ready? . ,char-ready?)
                (write . ,write)
                (display . ,display)
                (newline . ,newline)
                (write-char . ,write-char)
                (scheme-report-environment . ,scheme-report-environment)
                (null-environment . ,null-environment)
                (interaction-environment . ,interaction-environment)
                (number? . ,number?)
                (complex? . ,complex?)
                (real? . ,real?)
                (rational? . ,rational?)
                (integer? . ,integer?)
                (exact? . ,exact?)
                (inexact? . ,inexact?)
                (= . ,=)
                (< . ,<)
                (> . ,>)
                (<= . ,<=)
                (>= . ,>=)
                (zero? . ,zero?)
                (positive? . ,positive?)
                (negative? . ,negative?)
                (odd? . ,odd?)
                (even? . ,even?)
                (max . ,max)
                (min . ,min)
                (+ . ,+)
                (* . ,*)
                (- . ,-)
                (/ . ,/)
                (abs . ,abs)
                (quotient . ,quotient)
                (remainder . ,remainder)
                (modulo . ,modulo)
                (gcd . ,gcd)
                (lcm . ,lcm)
                (numerator . ,numerator)
                (denominator . ,denominator)
                (floor . ,floor)
                (ceiling . ,ceiling)
                (truncate . ,truncate)
                (round . ,round)
                (exp . ,exp)
                (log . ,log)
                (sin . ,sin)
                (cos . ,cos)
                (tan . ,tan)
                (asin . ,asin)
                (acos . ,acos)
                (atan . ,atan)
                (sqrt . ,sqrt)
                (expt . ,expt)
                (make-rectangular . ,make-rectangular)
                (make-polar . ,make-polar)
                (real-part . ,real-part)
                (imag-part . ,imag-part)
                (magnitude . ,magnitude)
                (angle . ,angle)
                (exact->inexact . ,exact->inexact)
                (inexact->exact . ,inexact->exact)
                (eval . ,(lambda (expression environment)
                           (eval expression environment)))
                (load . ,(lambda (file-name)
                           (call-with-input-file file-name
                             (lambda (input-port)
                               (let loop ((result (let ((foo 0)) (set! foo 0)))
                                          (expression (read input-port)))
                                 (if (eof-object? expression)
                                     result
                                     (loop (meta-eval expression environment)
                                           (read input-port))))))))))

    ;;
    ;; Not supported: rationalize
    ;;

    environment))
