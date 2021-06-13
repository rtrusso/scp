(need sasm/machdesc)

(define (nasm-x86-store-array)
  (machine-description
   ;; store-array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(perform (op store-array) (const (,label? array)) (const (,intconst? offset)) (const (,intconst? value))))
                (rewrite-rule "mov dword [~~~], 0x~"
                              (array label)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value x86-32-bit-intconst)))

   (instruction (input-pattern `(perform (op store-array) (const (,label? array)) (const (,intconst? offset)) (,register? value)))
                (rewrite-rule "mov dword [~~~], ~"
                              (array label)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value register)))

   (instruction (input-pattern `(perform (op store-array) (label (,label? array)) (const (,intconst? offset)) (,register? value)))
                (rewrite-rule "mov dword [~~~], ~"
                              (array label)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value register)))

   (instruction (input-pattern `(perform (op store-array) (,register? array) (const (,intconst? offset)) (,register? value)))
                (rewrite-rule "mov [~~~], ~"
                              (array register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value register)))

   (instruction (input-pattern `(perform (op store-array) (,register? array) (const (,intconst? offset)) (const (,intconst? value))))
                (rewrite-rule "mov dword [~~~], 0x~"
                              (array register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value x86-32-bit-intconst)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (,register? offset) (const (,intconst? value))))
                (rewrite-rule "mov dword [~~~], 0x~"
                              (pointer register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value x86-32-bit-intconst)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (const (,intconst? offset)) (label (,label? value))))
                (rewrite-rule "mov dword [~~~], ~"
                              (pointer register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value label)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (,register? offset) (label (,label? value))))
                (rewrite-rule "mov dword [~~~], ~"
                              (pointer register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value label)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (,register? offset) (,register? value)))
                (rewrite-rule "mov [~~~], ~"
                              (pointer register)
                              (offset nasm-x86-offset+/-)
                              (offset nasm-x86-offset)
                              (value register)))

   ))

