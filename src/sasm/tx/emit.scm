(define *sasm-assembler-output-port* #f)
(define (sasm-assembler-output-port)
  (if *sasm-assembler-output-port*
      *sasm-assembler-output-port*
      (current-output-port)))

(define (emit-label l)
  (emit-instruction (format "~:" (label l))))

(define (emit-instruction insn)
  (if (ends-with? insn ":")
      (begin (display insn
                      (sasm-assembler-output-port))
             (newline (sasm-assembler-output-port)))
      (begin (display "\t" (sasm-assembler-output-port))
             (display insn (sasm-assembler-output-port))
             (newline (sasm-assembler-output-port)))))

(define (emit-whitespace)
  (newline (sasm-assembler-output-port)))

