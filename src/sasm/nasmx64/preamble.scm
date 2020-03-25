(need sasm/machdesc)

(define (nasm-x64-preamble)
  (machine-description
   ;; preamble ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directive
   (instruction (input-pattern `(global-preamble))
                (rewrite-rule ("bits 64")
                              ("section .text")
                              ("")))

   (instruction (input-pattern `(data-preamble))
                (rewrite-rule ("")
                              ("section .data")
                              ("")))

   (instruction (input-pattern `(export (,label? label)))
                (rewrite-rule "global ~" (label label)))

   (instruction (input-pattern `(extern (,label? label)))
                (rewrite-rule "extern ~" (label label)))

   (instruction (input-pattern `(perform (op reserve-locals) (const (,intconst? n))))
                (rewrite-rule "sub rsp, ~"
                              (n (lambda (x) (* 8 x)))))

   (instruction (input-pattern `(perform (op function-preamble)))
                (rewrite-rule))
   ))

