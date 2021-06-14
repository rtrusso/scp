(need sasm/machdesc)

(define (nasm-x86-stack)
  (machine-description
   ;; push & pop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(push (const (,intconst? arg))))
                (rewrite-rule "push dword 0x~"
                              (arg x86-32-bit-intconst)))

   (instruction (input-pattern `(push (result (op load-array) (,register? ptr) (const (,intconst? offset)))))
                (rewrite-rule "push dword [~~~]"
                              (ptr register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)))

   (instruction (input-pattern `(push (,register? arg)))
                (rewrite-rule "push ~"
                              (arg register)))

   (instruction (input-pattern `(pop (const (,number? n))))
                (rewrite-rule "add esp, ~"
                              (n (lambda (x) (* x 4)))))

   (instruction (input-pattern `(pop (,register? dest)))
                (rewrite-rule "pop dword ~" (dest register)))

   ))

