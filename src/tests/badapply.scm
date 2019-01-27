(define (get-func x) x)
;; try to apply something that's not a function
(display ((get-func 3) 3))
(newline)
