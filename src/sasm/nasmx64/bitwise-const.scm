(need sasm/machdesc)

(define (nasm-x64-bitwise-const)
  (machine-description
   ;; bit-shift operations, constant offset

   (instruction (input-pattern `(assign (,register? dest) (op bit-lshift) (,register? src) (const (,intconst? n-bits))))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ("sal ~, ~" (dest register) (n-bits intconst))))

   (instruction (input-pattern `(assign (,register? dest) (op bit-rshift) (,register? src) (const (,intconst? n-bits))))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ("shr ~, ~" (dest register) (n-bits intconst))))

   (instruction (input-pattern `(assign (,register? dest) (op bit-arith-rshift) (,register? src) (const (,intconst? n-bits))))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ("sar ~, ~" (dest register) (n-bits intconst))))

   ))

