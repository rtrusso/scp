(need sasm/machdesc)

(define (nasm-x86-debug)
  (machine-description
   (instruction (input-pattern `(perform (op break)))
                (rewrite-rule "int 3"))
   ))

