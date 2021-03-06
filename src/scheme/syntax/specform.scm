(need scheme/base-syntax)
(need scheme/syntax/expand-context)

(define (expand-define exp env ctx)
;  (debug-expand "expand-define " exp)
  (if (or (in-expand-context? ctx 'context/top-level)
          (in-expand-context? ctx 'context/scan-internal-defines))
      (bind-value! env (define-symbol exp) (define-symbol exp)))
  (let ((val-exp
         (if (pair? (list-ref exp 1))
             (expand-lambda (define-code exp) env
                            (expand-context2 'context/define-lambda-value
                                            `((parent ,ctx))))
             (expand (define-code exp) env
                     (expand-context2 'context/define-value
                                      `((parent ,ctx)))))))
    (if (bad-binding? env (define-symbol exp))
        (begin (display "bad binding in expand-define")
               (newline)
               (display (define-symbol exp))
               (newline)
               (display (define-code exp))
               (newline)))
    `(define ,(begin (check-bad-binding "expand-define" env (define-symbol exp))
                     (rename-variable env (define-symbol exp)))
       ,val-exp)))

(define (expand-quote exp env ctx)
  (list 'quote (expand-immediate (quoted-value exp) env ctx)))

(define (lexical-environment locals parent-environment)
  (let ((vars (map (lambda (name)
                     (cons name
                           (bound-variable (if (generated-name? name)
                                               (generated-name-symbol name)
                                               name)
                                           (generate-unique-variable-id))))
                   locals)))
    (lambda (name . val)
      (define (lookup name)
        (cond ((assoc name vars) => cdr)
              (else (bound-value parent-environment name))))
      (define (bind obj)
        (if (symbol? obj)
            (bound-variable (if (generated-name? obj)
                                (generated-name-symbol obj)
                                name)
                            (generate-unique-variable-id))
            obj))
      (define (set name val)
        (set! vars (cons (cons name (bind val))
                         vars)))
      (if (null? val)
          (lookup name)
          (set name (car val))))))

(define (expand-lambda exp env ctx)
  (define (get-locals)
    (if (list? (lambda-args exp))
        (lambda-args exp)
        (improper->proper (lambda-args exp))))
;  (debug-expand "expand-lambda " exp)
  (let ((lexenv (lexical-environment (get-locals) env)))
    (let ((body (let ((body (expand-sequence (lambda-code exp)
                                             lexenv
                                             ctx)))
                  (if (and (pair? body)
                           (eqv? 'begin (car body)))
                      (cdr body)
                      (list body)))))
      (if (list? (lambda-args exp))
          `(lambda ,(map (lambda (sym)
                           (check-bad-binding "expand-lambda1" lexenv sym)
                           (rename-variable lexenv sym))
                         (lambda-args exp))
             ,@body)
          `(lambda ,(proper->improper
                     (map (lambda (sym)
                            (check-bad-binding "expand-lambda2" lexenv sym)
                            (rename-variable lexenv sym))
                          (improper->proper (lambda-args exp))))
             ,@body)))))

(define (expand-letrec exp env ctx)
;  (debug-expand "expand-letrec " exp)
  (let ((lexenv (lexical-environment (letrec-vars exp) env)))
    (let ((expanded-defns (map (lambda (var init)
                                 (check-bad-binding "expand-letrec" lexenv var)
                                 (list (rename-variable lexenv var)
                                       (expand init lexenv ctx)))
                               (letrec-vars exp)
                               (letrec-exps exp)))
          (body (let ((body (expand-sequence (letrec-body exp) lexenv ctx)))
                  (if (and (pair? body)
                           (eqv? 'begin (car body)))
                      (cdr body)
                      (list body)))))
      `(letrec ,expanded-defns ,@body))))

(define (expand-if exp env ctx)
;  (debug-expand "expand-if " exp)
  `(if ,(expand (if-predicate exp) env ctx)
       ,(expand (if-consequent exp) env ctx)
       ,(if (< (length exp) 4)
            ''()
            (expand (if-antecedent exp) env ctx))))

(define (expand-set exp env ctx)
  (if (bound-syntax? (set!-symbol exp) env)
      (error "Invalid syntax - attempt to set! bound syntax keyword" exp))
  (check-bad-binding "expand-set" env (set!-symbol exp))
  `(set! ,(rename-variable env (set!-symbol exp))
         ,(expand (set!-code exp) env ctx)))

(define (expand-begin exp env ctx)
;  (debug-expand "expand-begin " exp)
  (expand-sequence (begin-sequence exp)
                   env
                   ctx))

(define (expand-sequence exp env ctx)
  (define (seq-inline? exp)
    (and (list? exp)
         (not (null? exp))
         (eqv? 'begin (car exp))))
  (define (fix-inlines seq)
    (cond ((null? seq) seq)
          ((seq-inline? (car seq))
           (append (fix-inlines (cdar seq))
                   (fix-inlines (cdr seq))))
          (else (cons (car seq) (fix-inlines (cdr seq))))))
;  (debug-expand "expand-sequence " exp)
  (cond ((null? exp) (error "Null sequence"))
        ((= 1 (length exp)) (expand (car exp) env ctx))
        ((in-expand-context? ctx 'context/top-level)
         `(begin ,@(map (lambda (exp) (expand exp env ctx))
                        exp)))
        ((in-expand-context? ctx 'context/scan-internal-defines)
         (for-each (lambda (exp) (expand exp env ctx)) exp))
        ((in-expand-context? ctx 'context/sequence)
         `(begin ,@(map (lambda (exp) (expand exp env ctx))
                        exp)))
        (else
         (begin
           (for-each (lambda (exp)
                       (expand exp 
                               env 
                               (expand-context2 'context/scan-internal-defines
                                               `((parent ,ctx)))))
                     exp)
           `(begin ,@(map (lambda (exp) (expand exp env ctx))
                          exp))))))

(install-special-form 'define expand-define)
(install-special-form 'quote  expand-quote)
(install-special-form 'lambda expand-lambda)
(install-special-form 'if     expand-if)
(install-special-form 'set!   expand-set)
(install-special-form 'begin  expand-begin)
(install-special-form 'letrec expand-letrec)
