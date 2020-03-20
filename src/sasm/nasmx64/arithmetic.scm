(need sasm/machdesc)

(define (nasm-x64-arithmetic)
  (machine-description
   ;; arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(assign (,register? dest) (op add) (,register? dest) (const 1)))
                (rewrite-rule "inc ~" (dest register)))

   (instruction (input-pattern `(assign (,register? dest) (op sub) (,register? dest) (const 1)))
                (rewrite-rule "dec ~" (dest register)))

   (instruction (input-pattern `(assign (,register? dest) (op add-pointer) (,register? op-a) (const (,intconst? op-b))))
                (rewrite-rule ((assign (replace dest) (replace op-a)) (dest sym-register) (op-a sym-register))
                              ("add ~, ~"
                               (dest register)
                               (op-b (lambda (x) (* 8 x))))))

   (instruction (input-pattern `(assign (,register? dest) (op sub-pointer) (,register? op-a) (const (,intconst? op-b))))
                (rewrite-rule ((assign (replace dest) (replace op-a)) (dest sym-register) (op-a sym-register))
                              ("sub ~, ~"
                               (dest register)
                               (op-b (lambda (x) (* 8 x))))))

   ))

