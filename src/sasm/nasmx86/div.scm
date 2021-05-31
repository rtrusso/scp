(need sasm/machdesc)

(define (nasm-x86-divide)
  (machine-description
   ;; divide - binop w/ register constraints

   ;; (instruction (input-pattern `(assign (,x86-quotient? quotient-low)
   ;;                                      (op x86-hyg-div)
   ;;                                      (,x86-dividend-high? quotient-high)
   ;;                                      (,x86-dividend-low? quotient-low)
   ;;                                      (result (op load-array)
   ;;                                              (,register? ptr)
   ;;                                              (const (,integer? offset)))))
   ;;              (rewrite-rule ("imul dword [~~~]"
   ;;                             (ptr register)
   ;;                             (offset nasm-x86-offset+/-)
   ;;                             (offset nasm-x86-offset))))

   ; as clean as x86 divide gets. there's actually 2 return values, but SASM can't represent this yet!
   (instruction (input-pattern `(assign (,x86-quotient? dest) ; quotient eax, remainder edx
                                        (op x86-hyg-div)
                                        (,x86-dividend-high? x) ; edx
                                        (,x86-dividend-low? dest) ; eax
                                        (,x86-divisor-other-register? divisor))) ; some other register
                (rewrite-rule ("div ~" (divisor register))))

   ;; (instruction (input-pattern `(assign (,register? dest)
   ;;                                      (op div)
   ;;                                      (,register? op-a)
   ;;                                      (result (op load-array)
   ;;                                              (,register? ptr)
   ;;                                              (const (,integer? offset)))))
   ;;              (rewrite-rule ((assign (reg eax) (replace op-a)) (op-a sym-register))
   ;;                            ("imul dword [~~~]"
   ;;                             (ptr register)
   ;;                             (offset nasm-x86-offset+/-)
   ;;                             (offset nasm-x86-offset))
   ;;                            ((assign (replace dest) (reg eax)) (dest sym-register)))
   ;;              (side-effects (reg eax) (reg edx))
   ;;              (insel-rewrite-rule (assign (reg accum) (replace op-a))
   ;;                                  (assign (reg accum) (op hyg-mul) (reg accum)
   ;;                                          (result (op load-array)
   ;;                                                  (replace ptr)
   ;;                                                  (const (replace offset))))
   ;;                                  (assign (replace dest) (reg accum)) ))

   (instruction (input-pattern `(assign (,register? dest) (op div) (,const-or-reg? op-a) (,register? op-b)))
                (rewrite-rule ((assign (reg edx) (const 0)) ())
                              ((assign (reg eax) (replace op-a)) 
                               (op-a sym-register))
                              ("div ~" (op-b register))
                              ((assign (replace dest) (reg eax)) (dest sym-register))
                              )
                (side-effects (reg eax) (reg edx))
                (insel-rewrite-rule (assign (replace-temp x) (replace op-b))
                                    (assign (reg accum) (replace op-a))
                                    (assign (reg accum) (op x86-hyg-div) (reg index) (reg accum) (replace-temp x))
                                    (assign (replace dest) (reg accum)) ))

   ))
