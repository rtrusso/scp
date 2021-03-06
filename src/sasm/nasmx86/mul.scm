(need sasm/machdesc)

(define (nasm-x86-multiply)
  (machine-description
   ;; multiply - binop w/ register constraints

   (instruction (input-pattern `(assign (,x86-accumulator? dest)
                                        (op hyg-mul)
                                        (,x86-accumulator? dest)
                                        (result (op load-array)
                                                (,register? ptr)
                                                (const (,integer? offset)))))
                (rewrite-rule ("imul dword [~~~]"
                               (ptr register)
                               (offset nasm-x86-offset+/-)
                               (offset nasm-x86-offset))))

   (instruction (input-pattern `(assign (,x86-accumulator? dest) (op hyg-mul) (,x86-accumulator? dest) (,register? operand)))
                (rewrite-rule ("imul ~" (operand register))))

   (instruction (input-pattern `(assign (,register? dest)
                                        (op mul) 
                                        (,register? op-a) 
                                        (result (op load-array) 
                                                (,register? ptr)
                                                (const (,integer? offset)))))
                (rewrite-rule ((assign (reg eax) (replace op-a)) (op-a sym-register))
                              ("imul dword [~~~]" 
                               (ptr register)
                               (offset nasm-x86-offset+/-)
                               (offset nasm-x86-offset))
                              ((assign (replace dest) (reg eax)) (dest sym-register)))
                (side-effects (reg eax) (reg edx))
                (insel-rewrite-rule (assign (reg accum) (replace op-a))
                                    (assign (reg accum) (op hyg-mul) (reg accum)
                                            (result (op load-array)
                                                    (replace ptr)
                                                    (const (replace offset))))
                                    (assign (replace dest) (reg accum)) ))

   (instruction (input-pattern `(assign (,register? dest) (op mul) (,const-or-reg? op-a) (,const-or-reg? op-b)))
                (rewrite-rule ((assign (reg eax) (replace op-a)) (op-a sym-register))
                              ("imul ~" (op-b register))
                              ((assign (replace dest) (reg eax)) (dest sym-register)))
                (side-effects (reg eax) (reg edx))
                (insel-rewrite-rule (assign (replace-temp x) (replace op-b))
                                    (assign (reg accum) (replace op-a))
                                    (assign (reg accum) (op hyg-mul) (reg accum) (replace-temp x))
                                    (assign (replace dest) (reg accum)) ))

   (instruction (input-pattern `(assign (,register? dest) (op bit-not) (,const-or-reg? src)))
                (rewrite-rule ((assign (replace dest) (replace src)) (dest sym-register) (src sym-const-or-reg))
                              ("not ~" (dest register))))

   ))

