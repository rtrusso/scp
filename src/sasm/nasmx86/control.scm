(need sasm/machdesc)

(define (nasm-x86-control-simple)
  (machine-description
   ;; simple control flow ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(goto (label (,label? label))))
                (rewrite-rule "jmp NEAR ~" (label label)))

   (instruction (input-pattern `(goto (,const-or-reg? dest)))
                (rewrite-rule "jmp ~" (dest const-or-reg)))

   (instruction (input-pattern `(branch-true (label (,label? label))))
                (rewrite-rule "jnz NEAR ~" (label label)))

   (instruction (input-pattern `(branch-false (label (,label? label))))
                (rewrite-rule "jz NEAR ~" (label label)))

   (instruction (input-pattern `(branch-nonzero (label (,label? label))
                                                (,register? reg)))
                (rewrite-rule ("cmp ~, 0" (reg register))
                              ("jnz NEAR ~" (label label))
                              ))

   (instruction (input-pattern `(branch-zero (label (,label? label))
                                             (,register? reg)))
                (rewrite-rule ("cmp ~, 0" (reg register))
                              ("jz NEAR ~" (label label))
                              ))
   ))

(define (nasm-x86-control-complex)
  (machine-description
   (instruction (input-pattern `(branch-nonzero (label (,label? label))
                                                (result (op greater-than)
                                                        (,register? reg)
                                                        (const (,intconst? op-b)))))
                (rewrite-rule ("cmp ~, ~" (reg register) (op-b intconst))
                              ("jg NEAR ~" (label label))))

   (instruction (input-pattern `(branch-nonzero (label (,label? label))
                                                (result (op equal-to)
                                                        (,register? reg)
                                                        (const (,intconst? op-b)))))
                (rewrite-rule ("cmp ~, ~" (reg register) (op-b intconst))
                              ("je NEAR ~" (label label))))

   (instruction (input-pattern `(branch-nonzero (label (,label? label))
                                                (result (op greater-than)
                                                        (,register? reg-a)
                                                        (,register? reg-b))))
                (rewrite-rule ("cmp ~, ~" (reg-a register) (reg-b register))
                              ("jg NEAR ~" (label label))))

   (instruction (input-pattern `(branch-nonzero (label (,label? label))
                                                (result (op less-than-or-equal)
                                                        (,register? reg)
                                                        (const (,intconst? op-b)))))
                (rewrite-rule ("cmp ~, ~" (reg register) (op-b intconst))
                              ("jle NEAR ~" (label label))))

   ))

(define (nasm-x86-control)
  (append (nasm-x86-control-complex)
          (nasm-x86-control-simple)))
