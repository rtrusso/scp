(define *sasm-register-mapping*
  '((accum . accum)
    (operand . operand)
    (this . this)
    (index . index)))

(define *sasm-sys-register-mapping*
  '((stack-pointer . #f)
    (frame-pointer . #f)))

(define *sasm-caller-destroy-registers*
  '(this index operand))

(define (sasm-interp-specialize! target)
  (case target
    ((x86)
     (set! *sasm-register-mapping*
           (map (lambda (x) (cons (car x) (string->symbol (cdr x))))
                nasm-x86-registers))
     (set! *sasm-sys-register-mapping*
           (map (lambda (x) (cons (car x) (string->symbol (cdr x))))
                nasm-x86-sys-registers))
     (set! *sasm-caller-destroy-registers*
           '(ebx ecx edx edi esi)))
    ((x64)
     (set! *sasm-register-mapping*
           (map (lambda (x) (cons (car x) (string->symbol (cdr x))))
                nasm-x64-registers))
     (set! *sasm-sys-register-mapping*
           (map (lambda (x) (cons (car x) (string->symbol (cdr x))))
                nasm-x64-sys-registers))
     (set! *sasm-caller-destroy-registers*
           '(rbx rcx rdx rdi rsi r8 r9 r10 r11 r12 r13 r14 r15)))
    ((mips)
     (set! *sasm-register-mapping*
           (map (lambda (x) (cons (car x) (string->symbol (cdr x))))
                sasm-mips-registers))
     (set! *sasm-sys-register-mapping*
           (map (lambda (x) (cons (car x) (string->symbol (cdr x))))
                sasm-mips-sys-registers))
     (set! *sasm-caller-destroy-registers*
           '($t1 $t2 $t3 $v0 $v1 $a0 $a1 $a2 $a3 $a4
                 $t4 $t5 $t6 $t7 $s0 $s1 $s2 $s3 $s4
                 $s5 $s6 $s7 $t8)))
    (else
     (error "unknown target -- sasm-interp-specialize"))))

(define (sasm-interp-spec-register name)
  (cdr (assoc name *sasm-register-mapping*)))

(define (sasm-interp-spec-sys-register name)
  (cdr (assoc name *sasm-sys-register-mapping*)))
