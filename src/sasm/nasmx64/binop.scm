(need sasm/machdesc)

(define (nasm-x64-twoarg-binop-deref sasm-op mach-op)
  (machine-description

   ;; add rax, [rbx+N] ; N can be 0
   (instruction (input-pattern `(assign (,register? dest) 
                                        (op ,sasm-op) 
                                        (,register? op-a)
                                        (result (op load-array)
                                                (,register? ptr)
                                                (const (,intconst? offset)))))
                (rewrite-rule ((assign (replace dest) (replace op-a)) (dest sym-register) (op-a sym-register))
                              (,(format "~ ~, [~~~]" mach-op "~" "~" "~" "~")
                               (dest register)
                               (ptr register)
                               (offset nasm-x64-offset+/-)
                               (offset nasm-x64-offset))))

   ;; add rax, [rbx+8*rcx]
   (instruction (input-pattern `(assign (,register? dest) 
                                        (op ,sasm-op) 
                                        (,register? op-a)
                                        (result (op load-array)
                                                (,register? ptr)
                                                (,register? offset))))
                (rewrite-rule ((assign (replace dest) (replace op-a)) (dest sym-register) (op-a sym-register))
                              (,(format "~ ~, [~~~]" mach-op "~" "~" "~" "~")
                               (dest register)
                               (ptr register)
                               (offset nasm-x64-offset+/-)
                               (offset nasm-x64-offset))))

   ;; add [rax+N], rbx
   (instruction (input-pattern `(perform (op store-array)
                                         (,register? ptr)
                                         (const (,intconst? offset))
                                         (result (op ,sasm-op)
                                                 (result (op load-array)
                                                         (,register? ptr)
                                                         (const (,intconst? offset)))
                                                 (,register? op-b))))
                (rewrite-rule (,(format "~ [~~~], ~" mach-op "~" "~" "~" "~")
                               (ptr register)
                               (offset nasm-x64-offset+/-)
                               (offset nasm-x64-offset)
                               (op-b register))))

   ;; add [rax+8*rbx], rcx
   (instruction (input-pattern `(perform (op store-array)
                                         (,register? ptr)
                                         (,register? offset)
                                         (result (op ,sasm-op)
                                                 (result (op load-array)
                                                         (,register? ptr)
                                                         (,register? offset))
                                                 (,register? op-b))))
                (rewrite-rule (,(format "~ [~~~], ~" mach-op "~" "~" "~" "~")
                               (ptr register)
                               (offset nasm-x64-offset+/-)
                               (offset nasm-x64-offset)
                               (op-b register))))

   ))

(define (nasm-x64-twoarg-binop-direct sasm-op mach-op)
  (machine-description


   ;; add rax, N
   (instruction (input-pattern `(assign (,register? dest) (op ,sasm-op) (,register? op-a) (const (,intconst? op-b))))
                (rewrite-rule ((assign (replace dest) (replace op-a)) (dest sym-register) (op-a sym-register))
                              (,(format "~ ~, ~" mach-op "~" "~")
                               (dest register)
                               (op-b intconst))))

   ;; add rax, rbx [special case of (assign (reg dest) (op add) (reg dest) (reg op))]
   (instruction (input-pattern `(assign (,register? dest) (op ,sasm-op) (,register? dest) (,register? operand)))
                (rewrite-rule (,(format "~ ~, ~" mach-op "~" "~")
                               (dest register)
                               (operand register))))

   ;; add rax, rbx [general case]
   (instruction (input-pattern `(assign (,register? dest) (op ,sasm-op) (,const-or-reg? op-a) (,const-or-reg? op-b)))
                (rewrite-rule ((assign (replace dest) (replace op-a)) (dest sym-register) (op-a sym-const-or-reg))
                              (,(format "~ ~, ~" mach-op "~" "~")
                               (dest register)
                               (op-b const-or-reg)))
                (side-effects)
                (insel-rewrite-rule (assign (replace-temp x) (replace op-b))
                                    (assign (replace dest) (replace op-a))
                                    (assign (replace dest) (op ,sasm-op) (replace dest) (replace-temp x))))

   ))

(define (nasm-x64-standard-twoarg-binop sasm-op mach-op)
  (append (nasm-x64-twoarg-binop-deref sasm-op mach-op)
          (nasm-x64-twoarg-binop-direct sasm-op mach-op)))
