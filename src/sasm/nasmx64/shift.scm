(need sasm/machdesc)

(define (nasm-x64-rshift)
  (machine-description
   ;; bit-shift operations, variable offset

   ;; bit-arith-rshift

   (instruction (input-pattern `(assign (,register? dest) (op hyg-bit-lshift) (,register? src) (,register? bits)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ((assign (reg rcx) (replace bits)) (bits sym-register))
                              ("sal ~, cl" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op bit-lshift) (,const-or-reg? src) (,const-or-reg? bits)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-const-or-reg))
                              ((assign (reg rcx) (replace bits)) (bits sym-const-or-reg))
                              ("sal ~, cl" (dest register)))
                (side-effects (reg rcx))
                (insel-rewrite-rule (assign (replace dest) (op hyg-bit-lshift) (replace src) (replace bits))))

   (instruction (input-pattern `(assign (,register? dest) (op hyg-bit-rshift) (,register? src) (,register? bits)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ((assign (reg rcx) (replace bits)) (bits sym-register))
                              ("shr ~, cl" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op bit-rshift) (,register? src) (,register? bits)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ((assign (reg rcx) (replace bits)) (bits sym-register))
                              ("shr ~, cl" (dest register)))
                (side-effects (reg rcx))
                (insel-rewrite-rule (assign (replace dest) (op hyg-bit-rshift) (replace src) (replace bits))))

   (instruction (input-pattern `(assign (,register? dest) (op hyg-bit-arith-rshift) (,register? src) (,register? bits)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ((assign (reg rcx) (replace bits)) (bits sym-register))
                              ("sar ~, cl" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op bit-arith-rshift) (,register? src) (,register? bits)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-register))
                              ((assign (reg rcx) (replace bits)) (bits sym-register))
                              ("sar ~, cl" (dest register)))
                (side-effects (reg rcx))
                (insel-rewrite-rule (assign (replace dest) (op hyg-bit-arith-rshift) (replace src) (replace bits))))

   ))

