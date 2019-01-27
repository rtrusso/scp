;; sasm-insn.scm
;;
;; Routines for manipulating SASM instruction sequences.

(need scheme/tag)
(need pat/pat)

;; instruction sequences ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (insn-seq needs modifies insns)
  (tag 'insns (list needs modifies insns)))

(define (insn-seq:needed insns)
  (list-ref (safe-contents 'insns insns) 0))

(define (insn-seq:modified insns)
  (list-ref (safe-contents 'insns insns) 1))

(define (insn-seq:insns insns)
  (list-ref (safe-contents 'insns insns) 2))

(define (insn-seq:needs? insns reg)
  (memq reg (insn-seq:needed insns)))

(define (insn-seq:modifies? insns reg)
  (memq reg (insn-seq:modified insns)))

(define (empty-insn-seq)
  (insn-seq '() '() '()))

;; Labels and tokens ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define make-label-token
  (let ((serial 0))
    (lambda args
      (if (null? args)
          (set! serial (+ serial 1)))
      serial)))

(define (make-label tag token)
  (string->symbol (string-append (symbol->string tag) "-" (number->string token))))

(define make-temp-token
  (let ((serial 0))
    (lambda ()
      (set! serial (+ serial 1))
      serial)))

(define (make-temp tag token)
  (list 'temp token))

;; basic linkage routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (link-goto label)
  (tag '<linkage> label))

(define (goto-linkage-target linkage)
  (safe-contents '<linkage> linkage))

(define (link-return number-of-args)
  (tag '<linkage> (cons 'return number-of-args)))

(define (link-next)
  (tag '<linkage> 'next))

(define (goto-linkage label)
  (link-goto label))

(define (return-linkage n)
  (link-return n))

(define (next-linkage)
  (link-next))

(define (expression-linkage)
  (tag '<linkage> (vector #f)))

(define (linkage-return? linkage-obj)
  (and (tagged? '<linkage> linkage-obj)
       (let ((linkage (safe-contents '<linkage> linkage-obj)))
         (and (pair? linkage)
              (eqv? 'return (car linkage))
              (number? (cdr linkage))))))

(define (linkage-goto? linkage-obj)
  (and (tagged? '<linkage> linkage-obj)
       (let ((linkage (safe-contents '<linkage> linkage-obj)))
         (and (symbol? linkage)
              (not (pair? linkage))
              (not (eqv? 'return linkage))
              (not (eqv? 'next linkage))))))

(define (linkage-next? linkage)
  (and (tagged? '<linkage> linkage)
       (eqv? 'next (safe-contents '<linkage> linkage))))

(define (expression-linkage? linkage)
  (and (tagged? '<linkage> linkage)
       (let ((linkage (safe-contents '<linkage> linkage)))
         (vector? linkage))))

;; targets ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (expression-linkage-target linkage)
  (vector-ref (safe-contents '<linkage> linkage) 0))

(define (register-target reg)
  (tag '<target> `(reg ,reg)))

(define (make-target target)
  (tag '<target> target))

(define (targets-equal? target-a target-b)
  (equal? (safe-contents '<target> target-a)
          (safe-contents '<target> target-b)))

(define (null-target)
  (tag '<target> 'null))

(define (null-target? target)
  (eqv? 'null (safe-contents '<target> target)))

(define (immediate-target)
  (tag '<target> 'imm))

(define (immediate-target? target)
  (eqv? 'imm (safe-contents '<target> target)))

(define (exp-target-register? target)
  (pattern-match '(reg (? x)) (safe-contents '<target> target)))

(define (exp-target-register target-obj)
  (let ((target (safe-contents '<target> target-obj)))
    (if (exp-target-register? target-obj)
        (list-ref target 1)
        #f)))

(define (exp-target-is-specified-register? target register)
  (and (exp-target-register? target)
       (eqv? register (exp-target-register target))))

(define (exp-target-registers-used target)
  (if (exp-target-register? target)
      (list (exp-target-register target))
      '()))

(define (exp-target-registers-modified target)
  (if (exp-target-register? target)
      (list (exp-target-register target))
      '()))

(define (exp-assign-target target)
  (if (null-target? target)
      (error "assignment to null target -- exp-assign-target"))
  (safe-contents '<target> target))

;; instruction sequence combination ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (append-insn-seq . seqs)
  (define (append2 seq1 seq2)
    (insn-seq (union-lists eqv?
                           (insn-seq:needed seq1)
                           (subtract-lists eqv?
                                           (insn-seq:needed seq2)
                                           (insn-seq:modified seq1)))
              (union-lists eqv?
                           (insn-seq:modified seq1)
                           (insn-seq:modified seq2))
              (append (insn-seq:insns seq1)
                      (insn-seq:insns seq2))))
  (define (append-seqs seqs)
    (if (null? seqs)
        (empty-insn-seq)
        (append2 (car seqs)
                 (append-seqs (cdr seqs)))))
  (append-seqs seqs))

;; Used to append sequences where seq1 may "stomp on" registers
;; that have been initialized before seq1, and used by seq2.
;;
(define (preserving regs seq1 seq2)
  (if (null? regs)
      (append-insn-seq seq1 seq2)
      (let ((first-reg (car regs)))
        (if (and (insn-seq:needs? seq2 first-reg)
                 (insn-seq:modifies? seq1 first-reg))
            (preserving (cdr regs)
                        (insn-seq (union-lists eqv? (list first-reg) (insn-seq:needed seq1))
                                  (subtract-lists eqv? (insn-seq:modified seq1) (list first-reg))
                                  (append `((save (reg ,first-reg)))
                                          (insn-seq:insns seq1)
                                          `((restore (reg ,first-reg)))))
                        seq2)
            (preserving (cdr regs) seq1 seq2)))))

(define (preserving-loop-body regs body predicate)
  (if (null? regs)
      body
      (let ((first-reg (car regs)))
        (if (and (or (insn-seq:needs? predicate first-reg)
                     (insn-seq:needs? body first-reg))
                 (insn-seq:modifies? body first-reg))
            (preserving-loop-body (cdr regs)
                                  (insn-seq (insn-seq:needed body)
                                            (subtract-lists eqv? (insn-seq:modified body) (list first-reg))
                                            (append `((save (reg ,first-reg)))
                                                    (insn-seq:insns body)
                                                    `((restore (reg ,first-reg)))))
                                  predicate)
            (preserving-loop-body (cdr regs) body predicate)))))

(define (preserving-self regs seq)
  (if (null? regs)
      seq
      (let ((first-reg (car regs)))
        (if (and (insn-seq:needs? seq first-reg)
                 (insn-seq:modifies? seq first-reg))
            (preserving-self (cdr regs)
                             (insn-seq (insn-seq:needed seq)
                                       (subtract-lists eqv? (insn-seq:modified seq) (list first-reg))
                                       (append `((save (reg ,first-reg)))
                                               (insn-seq:insns seq)
                                               `((restore (reg ,first-reg))))))
            (preserving-self (cdr regs) seq)))))

;; Used to append two instruction sequences that are not related;
;; seq1 and seq2 are next to each other lexically in the assembly
;; code but not related at all by sequential execution.  An example
;; of this is when seq2 is the body of a lambda expression, and
;; goto expressions surround it.
;;
(define (tack-insn-seq seq1 seq2)
  (insn-seq
   (insn-seq:needed seq1)
   (insn-seq:modified seq2)
   (append (insn-seq:insns seq1) (insn-seq:insns seq2))))

;; Used when there is a branch, i.e. an 'if' statement
;;
(define (parallel-insn-seq seq1 seq2)
  (insn-seq
   (union-lists eqv? (insn-seq:needed seq1) (insn-seq:needed seq2))
   (union-lists eqv? (insn-seq:modified seq1) (insn-seq:modified seq2))
   (append (insn-seq:insns seq1) (insn-seq:insns seq2))))

;; basic code generation ;;

(define (codegen-linkage linkage-obj)
  (let ((linkage (safe-contents '<linkage> linkage-obj)))
    (cond
;   ((eqv? 'return linkage)
;     (insn-seq '() '() '((return))))
     ((linkage-return? linkage-obj)
      (insn-seq '() '() `((perform (op pop-frame))
                          (return (const ,(cdr linkage))))))
     ((linkage-next? linkage-obj)
      (empty-insn-seq))
     ((linkage-goto? linkage-obj)
      (insn-seq '() '() `((goto (label ,linkage)))))
     (else
      (error "Bad linkage specifier " linkage)))))

(define (with-linkage insns linkage)
  (preserving '()
              insns
              (codegen-linkage linkage)))

(define (display-insn-seq insn-seq)
  (define (display-spaced x) (display " ") (display x))
  (if (not (null? (insn-seq:needed insn-seq)))
      (begin (display ";; Needs   :")
             (for-each display-spaced (insn-seq:needed insn-seq))
             (newline) ))
  (if (not (null? (insn-seq:modified insn-seq)))
      (begin (display ";; Modifies:")
             (for-each display-spaced (insn-seq:modified insn-seq))
             (newline) ))
  (for-each (lambda (x) (display x) (newline))
            (insn-seq:insns insn-seq)))

(define (with-expression-linkage insns requested-target given-target linkage-obj)
  (if (not (and (tagged? '<target> requested-target)
                (tagged? '<target> given-target)))
      (error "invalid target types -- with-expression-linkage"))
  (if (and (not (expression-linkage? linkage-obj))
           (null-target? requested-target))
      (error "implicit target with expression linkage -- with-expression-linkage"))
  (if (null-target? given-target)
      (error "null given-target -- with-expression-linkage"))
  (if (expression-linkage? linkage-obj)
      (begin (if (not (or (null-target? requested-target)
                          (targets-equal? requested-target given-target)))
                 (error "expression linkage with mismatched targets -- with-expression-linkage"))
             (if (vector-ref (safe-contents '<linkage> linkage-obj) 0)
                 (error "linkage object is being re-used -- with-expression-linkage"))
             (vector-set! (safe-contents '<linkage> linkage-obj) 0 given-target)
             insns)
      (append-insn-seq insns
                       (if (targets-equal? given-target requested-target)
                           (empty-insn-seq)
                           (insn-seq (exp-target-registers-used given-target)
                                     (exp-target-registers-modified requested-target)
                                     `((assign ,(exp-assign-target requested-target)
                                               ,(exp-assign-target given-target)))) )
                       (codegen-linkage linkage-obj))))

(define (label-insn-seq label)
  (insn-seq '() '()
            `((label ,label))))

