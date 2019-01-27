;; sasm-assemble.scm
;; top-level assemble routines factored out of sasm-tx

(need sasm/sasm-tracing)

(define (sasm-assemble-port port)
  (debug sasm-assemble-port)
  (let* ((context (sasm-assemble-interactive-context))
         (read-result
          (debug before-sasm-read-port :display () (sasm-read-port context port))))
    (debug after-sasm-read-port)
    (sasm-assemble read-result)))

(define (sasm-assemble-file file)
  (sasm-assemble (sasm-read-file file)))

(define (sasm-assemble-class class-stmt)
  `((label ,(cadr class-stmt))
    ,@(map (lambda (data-object) `(data ,data-object))
           (cddr class-stmt))))

(define (assemble-instruction base-instruction)
; commented-out code to debug sasm on an instruction level
;  (display "assembling instruction: ")
;  (newline)
;  (display "    ")
;  (display base-instruction)
;  (newline)
  (let ((instruction (map (lambda (x)
                            (if (and (list? x)
                                     (= 2 (length x))
                                     (equal? 'symconst (car x))
                                     (symbol? (cadr x)))
                                (let ((entry (assoc (cadr x) *sasm-assemble-symconst-alist*)))
                                  (or (and entry `(const ,(cadr entry)))
                                      (error "Invalid symbolic constant -- " x)))
                                x))
                          base-instruction)))
    (let loop ((spec *machine-instructions*))
      (if (null? spec)
          (error "Unable to assemble SASM instruction -- ASSEMBLE-INSTRUCTION" instruction))
      (and (not (null? spec))
           (or (match-and-rewrite-instruction instruction (car spec))
               (loop (cdr spec)))))))

(define (assemble-instructions instructions)
  (for-each assemble-instruction 
            (filter (lambda (x) (not (sasm-tx-insn-trivial? x)))
                    instructions)))

