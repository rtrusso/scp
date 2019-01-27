(define nasm-x86-registers 
  '((accum . "eax") (operand . "ebx") (this . "ecx") (index . "edx")
    (link . "edi") (pointer . "esi")))

(define nasm-x86-sys-registers
  '((frame-pointer . "ebp") (stack-pointer . "esp")))

(define (nasm-x86-register-lsb-alias reg)
  (case (string->symbol (register reg))
    ((eax) "al")
    ((ebx) "bl")
    ((ecx) "cl")
    ((edx) "dl")
    (else (error "Invalid x86 register  -- NASM-X86-REGISTER-LSB-ALIAS" reg))))

(define (nasm-x86-offset+/- x)
  (cond ((symbol? x) "+")
        ((register? x) "+")
        ((negative? x) "-")
        ((positive? x) "+")
        ((zero? x) "")
        (else (error "invalid offset -- nasm-x86-offset+/-" x))))

(define (nasm-x86-offset x)
  (cond ((symbol? x) (format "4*~" x))
        ((register? x) (format "4*~" (register x)))
        ((zero? x) "")
        ((number? x) (* 4 (abs x)))
        (else (error "invalid offset -- nasm-x86-offset" x))))

(define (nasm-string s)
  (string-append
   "'"
   (escape-string (lambda (c)
                    (case c
                      ((#\\) '(#\\ #\\))
                      ((#\newline) '(#\\ #\n))
                      ((#\') '(#\\ #\'))
                      (else (list c))))
                  s)
   "',0"))

(define (x86-accumulator? x)
  (or (equal? x '(reg accum))
      (equal? x '(reg eax))))

