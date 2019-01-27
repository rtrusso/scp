;; sasm/sasm-mips.scm
;; MIPS machine description module for the SASM native code translator

(need util/string)
(need sasm/machdesc)

;(define (sasm-mips-label-transform label)
;  (string-append 
;   "_"
;   (escape-string (lambda (c)
;                    (case c
;                      ((#\$) '(#\_ #\D))
;                      ((#\?) '(#\_ #\Q))
;                      ((#\@) '(#\_ #\A))
;                      ((#\/) '(#\_ #\S))
;                      ((#\-) '(#\_))
;                      (else (list c))))
;                  (symbol->string label))))

(define sasm-mips-registers
  '((accum . "$t0") (operand . "$t1") (this . "$t2") (index . "$t3")
    (v0 . "$v0") (v1 . "$v1") 
    (a0 . "$a0") (a1 . "$a1") (a2 . "$a2") (a3 . "$a3")
    (link . "$t4") (pointer . "$t5") (t6 . "$t6") (t7 . "$t7")
    (s0 . "$s0") (s1 . "$s1") (s2 . "$s2") (s3 . "$s3")
    (s4 . "$s4") (s5 . "$s5") (s6 . "$s6") (s7 . "$s7")
    (t8 . "$t8"))) ;; (t9 . "$t9") is used as the "flags" register

(define sasm-mips-sys-registers
  '((frame-pointer . "$fp") (stack-pointer . "$sp")))

(define (mips-words n)
  (* 4 n))

(define (mips-intconst n)
  (if (<= n #x7fffffff)
      (number->string n 10)
      (string-append "0x" (number->string n 16))))

(define (mips-string s)
  (string-append
   "\""
   (escape-string (lambda (c)
                    (case c
                      ((#\\) '(#\\ #\\))
                      ((#\newline) '(#\\ #\n))
                      ((#\") '(#\\ #\"))
                      (else (list c))))
                  s)
   "\""))

(define sasm-mips-machine
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

   ;; directives  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(label (,label? label)))
                (rewrite-rule "~:" (label label)))

   (instruction (input-pattern `(data (const (,intconst? i))))
                (rewrite-rule ".word ~" (i intconst)))

   (instruction (input-pattern `(global (,label? label) (const :asciiz (,string? str))))
                (rewrite-rule ("~:" (label label))
                              (".asciiz ~" (str mips-string))))

   (instruction (input-pattern `(data (label (,label? l))))
                (rewrite-rule ".word ~" (l label)))

   (instruction (input-pattern `(global-preamble))
                (rewrite-rule ".text"))

   (instruction (input-pattern `(data-preamble))
                (rewrite-rule ".data"))

   (instruction (input-pattern `(export (,label? label)))
                (rewrite-rule ".globl ~" (label label)))

   (instruction (input-pattern `(extern (,label? label)))
                (rewrite-rule "## external symbol ~" (label label)))

   ;; frame manipulation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(perform (op reserve-locals) (const (,intconst? n))))
                (rewrite-rule "sub $sp, $sp, ~" (n mips-words)))

   ;; unconditional jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(goto (label (,label? label))))
                (rewrite-rule "j ~" (label label)))

   ;; it is assumed that (goto (reg x)) is only used for tail calls.
   (instruction (input-pattern `(goto (,register? dest)))
                (rewrite-rule ("lw $ra, 0($sp)")
                              ("addi $sp, $sp, 4")
                              ("jr ~" (dest register))))

   ;; conditional jump ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(branch-true (label (,label? label))))
                (rewrite-rule "bne $0, $t9, ~" (label label)))

   (instruction (input-pattern `(branch-false (label (,label? label))))
                (rewrite-rule "beq $0, $t9, ~" (label label)))

   (instruction (input-pattern `(branch-nonzero (label (,label? label))
                                                (,register? reg)))
                (rewrite-rule "bne $0, ~, ~" (reg register) (label label)))

   (instruction (input-pattern `(branch-zero (label (,label? label))
                                             (,register? reg)))
                (rewrite-rule "beq $0, ~, ~" (reg register) (label label)))

   ;; flags/booleans ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(test (op nonzero) (,register? reg)))
                (rewrite-rule "move $t9, ~" (reg register)))

   (instruction (input-pattern `(assign (,register? dest) (op less-than) (,register? op-a) (,register? op-b)))
                (rewrite-rule ("slt ~, ~, ~" (dest register) (op-a register) (op-b register))))

   (instruction (input-pattern `(assign (,register? dest) (op equal-to) (,register? op-a) (,register? op-b)))
                (rewrite-rule ("seq ~, ~, ~" (dest register) (op-a register) (op-b register))))

   (instruction (input-pattern `(assign (,register? dest) (op less-than-or-equal) (,register? op-a) (,register? op-b)))
                (rewrite-rule ("sle ~, ~, ~" (dest register) (op-a register) (op-b register))))

   ;; push & pop ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(push (,register? src)))
                (rewrite-rule ("sub $sp, $sp, 4")
                              ("sw ~, 0($sp)" (src register))))

   (instruction (input-pattern `(pop (const (,number? n))))
                (rewrite-rule ("add $sp, $sp, ~" (n mips-words))))

   (instruction (input-pattern `(pop (,register? dest)))
                (rewrite-rule ("lw ~, 0($sp)" (dest register))
                              ("add $sp, $sp, 4")))

   ;; store-array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(perform (op store-array) (,register? pointer) (const (,intconst? offset)) (,register? value)))
                (rewrite-rule "sw ~, ~(~)"
                              (value register)
                              (offset mips-words)
                              (pointer register)))

   (instruction (input-pattern `(perform (op store-array)
                                         (,register? pointer)
                                         (,register? offset)
                                         (,register? value)))
                (rewrite-rule
                 ("error - store-array rule not to be used after insel"))
                (side-effects)
                (insel-rewrite-rule (assign (replace-temp x) (op left-shift)
                                            (replace offset) (symconst shift-cells-per-word))
                                    (assign (replace-temp x) (op add)
                                            (replace-temp x) (replace pointer))
                                    (perform (op store-array) (replace-temp x)
                                             (const 0) (replace value))))

   ;; load-array ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(assign (,register? dest)
                                        (op load-array)
                                        (,register? pointer)
                                        (const (,intconst? offset))))
                (rewrite-rule "lw ~, ~(~)"
                              (dest register)
                              (offset mips-words)
                              (pointer register)))

   (instruction (input-pattern `(assign (,register? dest)
                                        (op load-array)
                                        (,register? pointer)
                                        (,register? offset)))
                (rewrite-rule
                 ("error - load-array rule not to be used after insel"))
                (side-effects)
                (insel-rewrite-rule (assign (replace-temp x) (op left-shift)
                                            (replace offset) (symconst shift-cells-per-word))
                                    (assign (replace-temp x) (op add)
                                            (replace-temp x) (replace pointer))
                                    (assign (replace dest) (op load-array)
                                            (replace-temp x) (const 0))))

   ;; function calls ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;   (instruction (input-pattern `(perform (op call) (const (,label? label))))
;                (rewrite-rule "jal ~" (label label)))

   (instruction (input-pattern `(perform (op call) (,register? pointer)))
                (rewrite-rule "jalr ~" (pointer register)))

   ;; return ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(return))
                (rewrite-rule ("lw $ra, 0($sp)")
                              ("add $sp, $sp, 4")
                              ("jr $ra")))

   (instruction (input-pattern `(return (const (,intconst? words))))
                (rewrite-rule ("lw $ra, 0($sp)")
                              ("add $sp, $sp, ~" (words (lambda (x) (mips-words (+ x 1)))))
                              ("jr $ra")))

   ;; frame directives ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(perform (op reserve-locals) (const (,intconst? n))))
                (rewrite-rule ("sub $sp, $sp, ~" (n mips-words))))

   (instruction (input-pattern `(perform (op function-preamble)))
                (rewrite-rule ("sub $sp, $sp, 4")
                              ("sw $ra, 0($sp)")))

   ;; assignment ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(assign (,register? dest) (const (,intconst? n))))
                (rewrite-rule "li ~, ~" (dest register) (n mips-intconst)))

   (instruction (input-pattern `(assign (,register? dest) (const (,label? label))))
                (rewrite-rule "la ~, ~" (dest register) (label label)))

   (instruction (input-pattern `(assign (,register? dest) (label (,label? label))))
                (rewrite-rule "la ~, ~" (dest register) (label label)))

   (instruction (input-pattern `(assign (,register? dest) (,register? src)))
                (rewrite-rule "move ~, ~" (dest register) (src register)))

   ;; arithmetic ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(assign (,register? dest) (op add) (,register? op-a) (,register? op-b)))
                (rewrite-rule "add ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op sub) (,register? op-a) (,register? op-b)))
                (rewrite-rule "sub ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op mul) (,register? op-a) (,register? op-b)))
                (rewrite-rule ("mult ~, ~" (op-a register) (op-b register))
                              ("mflo ~" (dest register))))

   (instruction (input-pattern `(assign (,register? dest) (op left-shift) (,register? op-a) (const (,intconst? n-bits))))
                (rewrite-rule "sll ~, ~, ~" (dest register) (op-a register) (n-bits intconst)))

   (instruction (input-pattern `(assign (,register? dest) (op add-pointer) (,register? pointer) (const (,intconst? words))))
                (rewrite-rule "addi ~, ~, ~" (dest register) (pointer register) (words mips-words)))

   (instruction (input-pattern `(assign (,register? dest) (op sub-pointer) (,register? pointer) (const (,intconst? words))))
                (rewrite-rule "addi ~, ~, -~" (dest register) (pointer register) (words mips-words)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-xor) (,register? op-a) (const (,intconst? mask))))
                (rewrite-rule "xori ~, ~, ~" (dest register) (op-a register) (mask intconst)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-xor) (,register? op-a) (,register? op-b)))
                (rewrite-rule "xor ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-or) (,register? op-a) (,register? op-b)))
                (rewrite-rule "or ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-and) (,register? op-a) (,register? op-b)))
                (rewrite-rule "and ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-lshift) (,register? op-a) (,register? op-b)))
                (rewrite-rule "sllv ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-rshift) (,register? op-a) (,register? op-b)))
                (rewrite-rule "srlv ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-arith-rshift) (,register? op-a) (,register? op-b)))
                (rewrite-rule "srav ~, ~, ~" (dest register) (op-a register) (op-b register)))

   (instruction (input-pattern `(assign (,register? dest) (op bit-not) (,register? src)))
                (rewrite-rule "not ~, ~" (dest register) (src register)))

   ;; system interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

   (instruction (input-pattern `(perform (op hyg-mips-system-call) (,register? reg-v) (,register? reg-a)))
                (rewrite-rule ((assign (reg $v0) (replace reg-v)) (reg-v sym-register))
                              ((assign (reg $a0) (replace reg-a)) (reg-a sym-register))
                              ("syscall")))

   (instruction (input-pattern `(perform (op mips-system-call) (,register? reg-v) (,register? reg-a)))
                (rewrite-rule ((assign (reg $v0) (replace reg-v)) (reg-v sym-register))
                              ((assign (reg $a0) (replace reg-a)) (reg-a sym-register))
                              ("syscall"))
                (side-effects (reg $v0) (reg $a0))
                (insel-rewrite-rule (perform (op hyg-mips-system-call) (replace reg-v) (replace reg-a))))

   ))

(define (sasm-set-target-mips!)
  (sasm-set-target! 'sasm-mips
                    sasm-mips-registers
                    sasm-mips-sys-registers
                    sasm-mips-machine)
  (set! *sasm-assemble-class-in-data-segment* #t)
;  (set! *sasm-backend-specific-label-transform*
;        sasm-mips-label-transform)
  )

(define (sasm-set-mips-hw-params!)
  (sasm-symconst-alist-append '((cells-per-word 4)
                                (shift-cells-per-word 2)
                                (mask-cells-per-word 3)
                                (bits-per-word 32)
                                (shift-bits-per-word 5)
                                (mask-bits-per-word 31))))
