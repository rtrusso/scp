(need sasm/machdesc)

(define (nasm-x86-interp-support)
  (machine-description
   ;; interpreter support ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(test (op interp-gc-not-a-number) (,register? reg)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-NOT-A-NUMBER ~"
                              (reg register)))

   (instruction (input-pattern `(test (op interp-gc-test-symbolic-pointer) (,register? reg)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-TEST-SYMBOLIC-POINTER ~"
                              (reg register)))

   (instruction (input-pattern `(assign (,register? dest) (op interp-gc-memory-area-size) (,register? reg)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-MEMORY-AREA-SIZE ~ ~"
                              (dest register)
                              (reg register)))

   (instruction (input-pattern `(assign (,register? dest) (op interp-gc-memory-ref) (,register? address) (,register? index)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-MEMORY-REF ~ ~ ~"
                              (dest register)
                              (address register)
                              (index register)))

   (instruction (input-pattern `(assign (,register? dest) (op interp-gc-auxiliary-stack)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-AUXILIARY-STACK ~"
                              (dest register)))

   (instruction (input-pattern `(assign (,register? dest) (op interp-gc-global-addrs)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-GLOBAL-ADDRS ~"
                              (dest register)))

   (instruction (input-pattern '(perform (op debug-break)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-DEBUG-BREAK"))

   (instruction (input-pattern `(perform (op debug-out) (,register? reg)))
                (rewrite-rule "@@@ERROR-INVALID-TO-ASSEMBLE-DEBUG-OUT"))
   ))

