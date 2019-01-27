(define (bound-syntax? symbol environment)
  (let ((val (bound-value environment symbol)))
    (and (not (free? val)) (not (bound? val)))))

(define (expand-variable-reference expression environment context)
  (if (bound-syntax? expression environment)
      (error "Invalid syntax "
             expression
             (bound-value environment expression)
             context))
  (check-bad-binding "expand-variable-reference" environment expression)
  (rename-variable environment expression))

