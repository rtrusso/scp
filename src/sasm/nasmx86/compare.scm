(need sasm/machdesc)

(define (nasm-x86-compare)
  (machine-description
   ;; flags/booleans ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(test (op nonzero) (,register? reg)))
                (rewrite-rule ("cmp ~, 0" (reg register))))

   (instruction (input-pattern `(assign (,register? dest) (op less-than) (,register? op-a) (const (,intconst? op-b))))
                (rewrite-rule ("cmp ~, ~" (op-a register) (op-b intconst))
                              ("setl ~" (dest nasm-x86-register-lsb-alias))
                              ("and ~, 1" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op less-than) (,const-or-reg? op-a) (,const-or-reg? op-b)))
                (rewrite-rule ("cmp ~, ~" (op-a const-or-reg) (op-b const-or-reg))
                              ("setl ~" (dest nasm-x86-register-lsb-alias))
                              ("and ~, 1" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op equal-to) (,register? op-a) (const (,intconst? op-b))))
                (rewrite-rule ("cmp ~, ~" (op-a register) (op-b intconst))
                              ("sete ~" (dest nasm-x86-register-lsb-alias))
                              ("and ~, 1" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op equal-to) (,register? op-a) (label (,label? op-b))))
                (rewrite-rule ("cmp ~, ~" (op-a register) (op-b label))
                              ("sete ~" (dest nasm-x86-register-lsb-alias))
                              ("and ~, 1" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op equal-to) (,const-or-reg? op-a) (,const-or-reg? op-b)))
                (rewrite-rule ("cmp ~, ~" (op-a const-or-reg) (op-b const-or-reg))
                              ("sete ~" (dest nasm-x86-register-lsb-alias))
                              ("and ~, 1" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op less-than-or-equal) (,register? op-a) (const (,intconst? op-b))))
                (rewrite-rule ("cmp ~, ~" (op-a register) (op-b intconst))
                              ("setle ~" (dest nasm-x86-register-lsb-alias))
                              ("and ~, 1" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op less-than-or-equal) (,register? op-a) (,register? op-b)))
                (rewrite-rule ("cmp ~, ~" (op-a register) (op-b register))
                              ("setle ~" (dest nasm-x86-register-lsb-alias))
                              ("and ~, 1" (dest register))))

   ))

