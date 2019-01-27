;; sasm code-generator configuration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; a symbol, identifying the target to the user
(define *machine-name* #f)
;; an association list of register symbols to actual register names
(define *machine-register-list* #f)
;; an association list of sys-register symbols to actual register names
(define *machine-sys-register-list* #f)
;; a table mapping SASM instruction patterns to actual assembly code
(define *machine-instructions* #f)

(define *sasm-assemble-class-in-data-segment* #f)

(define (sasm-set-target! name register-mapping sys-register-mapping machine)
  (set! *machine-name* name)
  (set! *machine-register-list* register-mapping)
  (set! *machine-sys-register-list* sys-register-mapping)
  (set! *machine-instructions* machine))

