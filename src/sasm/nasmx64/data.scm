(need sasm/machdesc)

(define (nasm-x64-data)
  (machine-description
   ;; data declaration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directive
   (instruction (input-pattern `(data (const (,intconst? i))))
                (rewrite-rule "dq ~" (i intconst)))

;; directive
   (instruction (input-pattern `(data (label (,label? l))))
                (rewrite-rule "dq ~" (l label)))

   (instruction (input-pattern `(data (const (,string? str))))
                (rewrite-rule "db ~" (str nasm-string)))

   ))

