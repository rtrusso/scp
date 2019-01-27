;; scheme/transforms/internal-defines.scm
;; A translation pass that turns internal definitions in lambda expressions into
;; letrec expressions.

(need scheme/base-syntax)

(define (rewrite-defines code)
  (define (iter input defines code)
    (if (null? input)
        (cond ((null? defines)
               (map rewrite-defines (reverse code)))
              (else
               `((letrec ,(map list
                               (map define-symbol defines)
                               (map rewrite-defines (map define-code defines)))
                   ,@(map rewrite-defines (reverse code))))))
        (let ((expr (car input)))
          (if (and (pair? expr)
                   (eqv? 'define (car expr)))
              (iter (cdr input) (cons expr defines) code)
              (iter (cdr input) defines (cons expr code))))))
  (if (not (pair? code))
      code
      (case (car code)
        ((quote) code)
        ((define) `(define ,(define-symbol code)
                     ,(rewrite-defines (define-code code))))
        ((if) `(if ,(rewrite-defines (if-predicate code))
                   ,(rewrite-defines (if-consequent code))
                   ,(rewrite-defines (if-antecedent code))))
        ((begin) `(begin ,@(map rewrite-defines (begin-sequence code))))
        ((set!) `(set! ,(set!-symbol code)
                       ,(rewrite-defines (set!-code code))))
        ((letrec)
         `(letrec ,(map (lambda (var init)
                          (list var (rewrite-defines init)))
                        (letrec-vars code)
                        (letrec-exps code))
            ,@(iter (letrec-body code) '() '())))
        ((lambda) `(lambda ,(lambda-args code)
                     ,@(iter (lambda-code code) '() '())))
        (else (map rewrite-defines code)))))
