(need sasm/machdesc)

(define (nasm-x86-load-array)
  (machine-description
   ;; load-array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(assign (,register? dest) (op load-array) (const (,label? array)) (const (,intconst? offset))))
                (rewrite-rule "mov ~, [~~~]"
                              (dest register)
                              (array label)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)))

   (instruction (input-pattern `(assign (,register? dest) (op load-array) (label (,label? array)) (const (,intconst? offset))))
                (rewrite-rule "mov ~, [~~~]"
                              (dest register)
                              (array label)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)))

   (instruction (input-pattern `(assign (,register? dest) (op load-array) (,register? array) (const (,intconst? offset))))
                (rewrite-rule "mov ~, [~~~]"
                              (dest register)
                              (array register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)))

   (instruction (input-pattern `(assign (,register? dest) (op load-array) (,const-or-reg? array) (,const-or-reg? index)))
                (rewrite-rule "mov ~, [~~~]" 
                              (dest register)
                              (array const-or-reg)
                              (index nasm-x86-offset+/-)
                              (index nasm-x86-offset)))

   ))


