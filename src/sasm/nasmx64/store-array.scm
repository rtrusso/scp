(need sasm/machdesc)

(define (nasm-x64-store-array)
  (machine-description
   ;; store-array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(perform (op store-array) (const (,label? array)) (const (,intconst? offset)) (const (,intconst? value))))
                (rewrite-rule "mov qword [~~~], ~"
                              (array label)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value intconst)))

   (instruction (input-pattern `(perform (op store-array) (const (,label? array)) (const (,intconst? offset)) (,register? value)))
                (rewrite-rule "mov qword [~~~], ~"
                              (array label)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value register)))

   (instruction (input-pattern `(perform (op store-array) (label (,label? array)) (const (,intconst? offset)) (,register? value)))
                (rewrite-rule "mov qword [~~~], ~"
                              (array label)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value register)))

   (instruction (input-pattern `(perform (op store-array) (,register? array) (const (,intconst? offset)) (,register? value)))
                (rewrite-rule "mov [~~~], ~"
                              (array register)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value register)))

   (instruction (input-pattern `(perform (op store-array) (,register? array) (const (,intconst? offset)) (const (,intconst? value))))
                (rewrite-rule "mov qword [~~~], ~"
                              (array register)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value intconst)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (,register? offset) (const (,intconst? value))))
                (rewrite-rule "mov qword [~~~], ~"
                              (pointer register)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value intconst)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (const (,intconst? offset)) (label (,label? value))))
                (rewrite-rule "mov qword [~~~], ~"
                              (pointer register)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value label)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (,register? offset) (label (,label? value))))
                (rewrite-rule "mov qword [~~~], ~"
                              (pointer register)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value label)))

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (,register? offset) (,register? value)))
                (rewrite-rule "mov [~~~], ~"
                              (pointer register)
                              (offset nasm-x64-offset+/-)
                              (offset nasm-x64-offset)
                              (value register)))

   ))

