(need sasm/machdesc)

(define (nasm-x64-stack)
  (machine-description
   ;; push & pop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(push (const (,intconst? arg))))
                (rewrite-rule "push qword ~"
                              (arg intconst)))

   (instruction (input-pattern `(push (result (op load-array) (,register? ptr) (const (,intconst? offset)))))
                (rewrite-rule "push qword [~~~]"
                              (ptr register)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)))

   (instruction (input-pattern `(push (,register? arg)))
                (rewrite-rule "push ~"
                              (arg register)))

   (instruction (input-pattern `(pop (const (,number? n))))
                (rewrite-rule "add rsp, ~"
                              (n (lambda (x) (* x 8)))))

   (instruction (input-pattern `(pop (,register? dest)))
                (rewrite-rule "pop qword ~" (dest register)))

   ))

