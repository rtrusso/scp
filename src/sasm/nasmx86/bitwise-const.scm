(need sasm/machdesc)

(define (nasm-x86-bitwise-const)
  (machine-description
   ;; bit-shift operations, constant offset

   (instruction (input-pattern `(assign (,register? dest) (op bit-lshift) (,register? src) (const (,intconst? n-bits))))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ("sal ~, 0x~" (dest register) (n-bits x86-32-bit-intconst))))

   (instruction (input-pattern `(assign (,register? dest) (op bit-rshift) (,register? src) (const (,intconst? n-bits))))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ("shr ~, 0x~" (dest register) (n-bits x86-32-bit-intconst))))

   (instruction (input-pattern `(assign (,register? dest) (op bit-arith-rshift) (,register? src) (const (,intconst? n-bits))))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ("sar ~, 0x~" (dest register) (n-bits x86-32-bit-intconst))))

   ))

