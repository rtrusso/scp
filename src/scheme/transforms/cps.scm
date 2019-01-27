(need scheme/base-syntax)
(need util/symbol)

(define cps-top-level-cont '~@@cont@@~)

(define (cps-rename symbol)
  (symbol-append 'user@ symbol))

(define cps-gentmp
  (let ((count -1))
    (lambda ()
      (set! count (+ count 1))
      (string->symbol (string-append "t" (number->string count))))))

(define cps-gencont
  (let ((count -1))
    (lambda ()
      (set! count (+ count 1))
      (string->symbol (string-append "k" (number->string count))))))

(define (cps-top-level exp)
  (cps exp '() cps-top-level-cont))

;;; Convert a Scheme expression to an equivalent CPS representation.
;;; 'code' A scheme expression, represented as an sexpr.
;;; 'cont' The continuation that the result of evaluation is delivered to.
;;;        This is represented as a name.

(define (cps code env cont)
  (cond ((cps-trivial? code) (cps-trivial code env cont))
        ((test-begin? code) (cps-begin code env cont))
        ((test-if? code) (cps-if code env cont))
        ((test-set!? code) (cps-set code env cont))
        ((test-lambda? code) (cps-lambda code env cont))
        ((test-application? code) (cps-application code env cont))
        (else (error "CPS - unrecognized code " code))))

(define (cps-trivial? code)
  (or (symbol? code)
      (not (pair? code))
      (test-immediate? code)
      (test-quoted? code)))

(define (cps-trivial-inline code)
  (cond ((symbol? code) (cps-rename code))
        ;((test-quoted? code) (quoted-value code))
        (else code)))

(define (cps-trivial code env cont)
  `(,cont
    ,(cps-trivial-inline code)))

(define (cps-if code env cont)
  (let ((predicate (if-predicate code))
        (consequent (if-consequent code))
        (antecedent (if-antecedent code)))
    (if (cps-trivial? predicate)
        `(if ,(cps-trivial-inline predicate)
             ,(cps consequent env cont)
             ,(cps antecedent env cont))
        (let ((if-cont-var (cps-gencont)))
          (let ((if-cont `(lambda (,if-cont-var)
                            (if ,if-cont-var
                                ,(cps consequent env cont)
                                ,(cps antecedent env cont)))))
            (cps predicate env if-cont))))))

(define (cps-set code env cont)
  (let ((symbol (cps-rename (set!-symbol code)))
        (exp (set!-code code)))
    (if (cps-trivial? exp)
        `(,cont (set! ,symbol ,(cps-trivial-inline exp)))
        (let ((set-cont-var (cps-gencont)))
          (let ((set-cont `(lambda (,set-cont-var)
                             (,cont (set! ,symbol ,set-cont-var)))))
            (cps exp env set-cont))))))

(define (cps-begin code env cont)
  (define (inside code)
    (if (null? (cdr code))
        (cps (car code) env cont)
        (let ((tmp (cps-gentmp)))
          (cps (car code) env `(lambda (,tmp)
                                 ,(inside (cdr code)))))))
  (inside (begin-sequence code)))

(define (cps-lambda code env cont)
  `(,cont
    ,(let ((args (lambda-args code))
           (exp `(begin ,@(lambda-code code)))
           (kn (cps-gencont)))
       `(lambda ,(cons kn (map cps-rename args))
          ,(cps exp (append args env) kn)))))

(define (cps-application code env cont)
  (letrec ((op (application-op code))
           (args (application-args code)))
    (define (cps-op temps)
      (if (cps-trivial? op)
          `(,(cps-trivial-inline op) ,cont ,@temps)
          (let ((tmp (cps-gentmp)))
            (cps op env `(lambda (,tmp)
                           (,tmp ,cont ,@temps))))))
    (define (cps-args temps args)
      (cond ((null? args) (cps-op (reverse temps)))
            ((cps-trivial? (car args))
             (cps-args (cons (cps-trivial-inline (car args)) temps)
                       (cdr args)))
            (else
             (let ((tmp (cps-gentmp)))
               (cps (car args) env `(lambda (,tmp)
                                      ,(cps-args (cons tmp temps)
                                                 (cdr args))))))))
    (cps-args '() args)))
