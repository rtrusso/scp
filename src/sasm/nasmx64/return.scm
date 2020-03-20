(need sasm/machdesc)

(define (nasm-x64-return)
  (machine-description
   ;; return ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; primary
   (instruction (input-pattern `(return))
                (rewrite-rule ("ret")))

   (instruction (input-pattern `(return (const (,intconst? val))))
                (rewrite-rule ("ret ~" (val (lambda (x) (* 8 x))))))

   ;; assignment expressions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(assign (,register? dest) (const (,(lambda (x) (or (label? x) (number? x))) c))))
                (rewrite-rule "mov ~, ~"
                              (dest register)
                              (c (lambda (x) (cond ((label? x) (label x)) ((number? x) x) (else (error "bad constant")))))))

   (instruction (input-pattern `(assign (,register? dest) (label (,label? label))))
                (rewrite-rule "mov ~, ~"
                              (dest register)
                              (label label)))

   (instruction (input-pattern `(assign (,register? dest) (,const-or-reg? src)))
                (rewrite-rule "mov ~, ~"
                              (dest register)
                              (src const-or-reg)))

   ))

