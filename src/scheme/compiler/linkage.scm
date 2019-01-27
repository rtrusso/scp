(define (link-register)
  '(reg link))


(define (env-register)
  '(reg this))


(define (scheme-preserved-registers)
  (append (target-modifies (link-register))
          (target-modifies (env-register))))


(define (scheme-continue-linkage n-args)
  `(continue ,n-args))


(define (scheme-continue-linkage? linkage)
  (and (pair? linkage)
       (eqv? 'continue (car linkage))))


(define (scheme-continue-linkage-arg-count linkage)
  (cadr linkage))


(define (with-scheme-linkage insns linkage)
  (preserving
   (scheme-preserved-registers)
   insns
   (cond ((scheme-continue-linkage? linkage)
          (if (number? (scheme-continue-linkage-arg-count linkage))
              (scheme-codegen-continue-with-accum-fixed-args
               (scheme-continue-linkage-arg-count linkage))
              (scheme-codegen-continue-with-accum-varargs)))
         (else
          (codegen-linkage linkage)))))


