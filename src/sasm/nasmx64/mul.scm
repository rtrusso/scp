(need sasm/machdesc)

(define (nasm-x64-multiply)
  (machine-description
   ;; multiply - binop w/ register constraints

   (instruction (input-pattern `(assign (,x64-accumulator? dest)
                                        (op hyg-mul)
                                        (,x64-accumulator? dest)
                                        (result (op load-array)
                                                (,register? ptr)
                                                (const (,integer? offset)))))
                (rewrite-rule ("imul qword [~~~]"
                               (ptr register)
                               (offset nasm-x64-offset+/-)
                               (offset nasm-x64-offset))))

   (instruction (input-pattern `(assign (,x64-accumulator? dest) (op hyg-mul) (,x64-accumulator? dest) (,register? operand)))
                (rewrite-rule ("imul ~" (operand register))))

   (instruction (input-pattern `(assign (,register? dest)
                                        (op mul) 
                                        (,register? op-a) 
                                        (result (op load-array) 
                                                (,register? ptr)
                                                (const (,integer? offset)))))
                (rewrite-rule ((assign (reg rax) (replace op-a)) (op-a sym-register))
                              ("imul qword [~~~]" 
                               (ptr register)
                               (offset nasm-x64-offset+/-)
                               (offset nasm-x64-offset))
                              ((assign (replace dest) (reg rax)) (dest sym-register)))
                (side-effects (reg rax) (reg rdx))
                (insel-rewrite-rule (assign (reg accum) (replace op-a))
                                    (assign (reg accum) (op hyg-mul) (reg accum)
                                            (result (op load-array)
                                                    (replace ptr)
                                                    (const (replace offset))))
                                    (assign (replace dest) (reg accum)) ))

   (instruction (input-pattern `(assign (,register? dest) (op mul) (,const-or-reg? op-a) (,const-or-reg? op-b)))
                (rewrite-rule ((assign (reg rax) (replace op-a)) (op-a sym-register))
                              ("imul ~" (op-b register))
                              ((assign (replace dest) (reg rax)) (dest sym-register)))
                (side-effects (reg rax) (reg rdx))
                (insel-rewrite-rule (assign (replace-temp x) (replace op-b))
                                    (assign (reg accum) (replace op-a))
                                    (assign (reg accum) (op hyg-mul) (reg accum) (replace-temp x))
                                    (assign (replace dest) (reg accum)) ))

   (instruction (input-pattern `(assign (,register? dest) (op bit-not) (,const-or-reg? src)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-const-or-reg))
                              ("not ~" (dest register))))

   ))

