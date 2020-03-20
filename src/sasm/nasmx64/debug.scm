(need sasm/machdesc)

(define (nasm-x64-debug)
  (machine-description
   (instruction (input-pattern `(perform (op break)))
                (rewrite-rule "int 3"))
   ))

