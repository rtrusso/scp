(need sasm/machdesc)

(define (nasm-x86-call)
  (machine-description
   ;; calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; primary
   (instruction (input-pattern `(perform (op call) (const (,label? label))))
                (rewrite-rule ("call ~" (label label))))

   (instruction (input-pattern `(perform (op call) (label (,label? label))))
                (rewrite-rule ("call ~" (label label))))

;; primary?
   (instruction (input-pattern `(perform (op call) (,register? pointer)))
                (rewrite-rule ("call ~" (pointer register))))

   (instruction (input-pattern `(perform (op tail-call) (const (,label? label)) (const (,intconst? in)) (const (,intconst? out))))
                (rewrite-rule ("call ~" (label label))))

   (instruction (input-pattern `(perform (op tail-call) (,register? pointer) (const (,intconst? in)) (const (,intconst? out))))
                (rewrite-rule ("call ~" (pointer register))))

   ))

