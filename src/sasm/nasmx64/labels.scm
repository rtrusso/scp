(need sasm/machdesc)

(define (nasm-x64-machine-labels)
  (machine-description
   ;; labels ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   ;; directive
   (instruction (input-pattern `(label (,label? label)))
                (rewrite-rule "~:" (label label)))

   ))

