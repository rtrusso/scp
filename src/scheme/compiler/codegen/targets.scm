(define (target-modifies target)
  (if (not (pair? target))
      (begin (display "Bad target: ")
             (write target)
             (newline)
             (error "Bad target" target)))
  (if (and (eqv? 'reg (car target))
           (null? (cddr target)))
      (list (cadr target))
      (error "Invalid target specification" target)))

;   ;; application of @continue
;   ((and (pair? expr)
;         (pair? (cdr expr))
;         (null? (cddr expr))
;         (eqv? '@continue (car expr)))
;    (scheme-codegen (cadr expr) env target linkage))

