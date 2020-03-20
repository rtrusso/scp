(define nasm-x64-registers 
  '((accum . "rax") (operand . "rbx") (this . "rcx") (index . "rdx")
    (link . "rdi") (pointer . "rsi")
    (r8 . "r8") (r9 . "r9") (r10 . "r10") (r11 . "r11")
    (r12 . "r12") (r13 . "r13") (r14 . "r14") (r15 . "r15")
    ))

(define nasm-x64-sys-registers
  '((frame-pointer . "rbp") (stack-pointer . "rsp")))

(define (nasm-x64-register-lsb-alias reg)
  (case (string->symbol (register reg))
    ((eax) "al")
    ((ebx) "bl")
    ((ecx) "cl")
    ((edx) "dl")
    (else (error "Invalid x64 register  -- NASM-X64-REGISTER-LSB-ALIAS" reg))))

(define (nasm-x64-offset+/- x)
  (cond ((symbol? x) "+")
        ((register? x) "+")
        ((negative? x) "-")
        ((positive? x) "+")
        ((zero? x) "")
        (else (error "invalid offset -- nasm-x64-offset+/-" x))))

(define (nasm-x64-offset x)
  (cond ((symbol? x) (format "8*~" x))
        ((register? x) (format "8*~" (register x)))
        ((zero? x) "")
        ((number? x) (* 8 (abs x)))
        (else (error "invalid offset -- nasm-x64-offset" x))))

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

(define (x64-accumulator? x)
  (or (equal? x '(reg accum))
      (equal? x '(reg rax))))

