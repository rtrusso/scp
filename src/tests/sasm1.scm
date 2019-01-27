(define *sasm-output-progress* #t)

(define (filter-map good list)
  (define (iter result list)
    (if (null? list)
        (reverse result)
        (let ((t (good (car list))))
          (iter (if t (cons t result) result) (cdr list)))))
  (iter '() list))

(define (sasm-local-max sasm-code)
  (let* (
         (unused-0     (if *sasm-output-progress*
                           (begin
                             (display ";; sasm-local-max: before nums"
                                      (current-output-port))
                             (newline (current-output-port))
                             )))
         (nums (filter-map (lambda (x)
                            (and (list? x)
                                 (= 2 (length x))
                                 (or (equal? 'local (car x))
                                     (equal? 'n-locals (car x)))
                                 (+ 1 (cadr x))))
                           (apply append sasm-code)))
         (unused-1     (if *sasm-output-progress*
                           (begin
                             (display ";; sasm-local-max: before decl"
                                      (current-output-port))
                             (newline (current-output-port))
                             )))

        (decl (apply max 
                     0
                     (filter-map (lambda (x)
                                   (and (equal? 'perform (car x))
                                        (equal? '(op reserve-locals) (list-ref x 1))
                                        (equal? 'const (car (list-ref x 2)))
                                        (cadr (list-ref x 2))))
                                 sasm-code))))
    (if *sasm-output-progress*
        (begin
          (display ";; sasm-local-max: before last apply"
                   (current-output-port))
          (newline (current-output-port))
          ))
    (display "decl: ")
    (write decl)
    (newline)
    (newline)
    (display "nums: ")
    (write nums)
    (newline)
    (newline)
    (apply max decl nums)))

(define *opt-code* '((push (reg edx)) (push (sys frame-pointer)) (assign (sys frame-pointer) (sys stack-pointer)) (perform (op reserve-locals) (const 4)) (branch-nonzero (label arg-check-success-1) (result (op greater-than) (reg edx) (const 0))) (assign (reg eax) (const 1)) (push (reg eax)) (push (reg edx)) (perform (op call) (label scheme-rtl-runtime-argument-count-error)) (label arg-check-success-1) (perform (op store-array) (sys frame-pointer) (const -1) (reg edx)) (perform (op store-array) (sys frame-pointer) (const -2) (reg ecx)) (perform (op store-array) (sys frame-pointer) (const -3) (reg edi)) (push (const 3)) (perform (op call) (label cp-rtl-array-malloc)) (assign (reg edi) (op load-array) (sys frame-pointer) (const -3)) (assign (reg ecx) (op load-array) (sys frame-pointer) (const -2)) (assign (reg edx) (op load-array) (sys frame-pointer) (const -1)) (perform (op store-array) (reg eax) (const 0) (reg ecx)) (assign (reg ecx) (reg eax)) (perform (op store-array) (sys frame-pointer) (const -1) (reg edx)) (perform (op store-array) (sys frame-pointer) (const -2) (reg ecx)) (perform (op store-array) (sys frame-pointer) (const -3) (reg edi)) (assign (reg eax) (label $scmliteral-nil)) (label varargs-copy-begin-2) (branch-nonzero (label varargs-copy-end-2) (result (op less-than-or-equal) (reg edx) (const 1))) (perform (op store-array) (sys frame-pointer) (const -4) (reg edx)) (push (reg eax)) (assign (reg ebx) (op add) (reg edx) (const 1)) (assign (reg ebx) (op bit-lshift) (reg ebx) (const 2)) (assign (reg edx) (op add) (sys frame-pointer) (reg ebx)) (assign (reg eax) (op load-array) (reg edx) (const 0)) (push (reg eax)) (assign (reg edi) (label varargs-copy-link-2)) (assign (reg eax) (label scmglue-cons)) (assign (reg edx) (const 2)) (goto (reg eax)) (label varargs-copy-link-2) (assign (reg edx) (op load-array) (sys frame-pointer) (const -4)) (assign (reg edx) (op sub) (reg edx) (const 1)) (goto (label varargs-copy-begin-2)) (label varargs-copy-end-2) (assign (reg edi) (op load-array) (sys frame-pointer) (const -3)) (assign (reg ecx) (op load-array) (sys frame-pointer) (const -2)) (assign (reg edx) (op load-array) (sys frame-pointer) (const -1)) (perform (op store-array) (reg ecx) (const 2) (reg eax)) (assign (reg eax) (op load-array) (sys frame-pointer) (const 2)) (perform (op store-array) (reg ecx) (const 1) (reg eax)) (perform (op store-array) (sys frame-pointer) (const -1) (reg edi)) (label lexical-reference-bound_00000003@args-3) (assign (reg eax) (op load-array) (reg ecx) (const 2)) (branch-nonzero (label scheme-arg-check-link-4) (reg eax)) (push (const 214)) (perform (op break)) (label scheme-arg-check-link-4) (push (reg eax)) (label lexical-reference-bound_00000002@arg1-5) (assign (reg eax) (op load-array) (reg ecx) (const 1)) (branch-nonzero (label scheme-arg-check-link-6) (reg eax)) (push (const 246)) (perform (op break)) (label scheme-arg-check-link-6) (push (reg eax)) (assign (reg ebx) (op load-array) (label $scmglobal-cons) (const 0)) (assign (reg ecx) (op load-array) (reg ebx) (const 1)) (branch-nonzero (label function-check-7) (result (op equal-to) (reg ecx) (const -2147483639))) (perform (op break)) (label function-check-7) (assign (reg ecx) (op load-array) (reg ebx) (const 2)) (assign (reg ebx) (op load-array) (reg ebx) (const 3)) (assign (reg edi) (label scheme-apply-link-8)) (assign (reg edx) (const 2)) (goto (reg ebx)) (label scheme-apply-link-8) (branch-nonzero (label scheme-arg-check-link-9) (reg eax)) (push (const 280)) (perform (op break)) (label scheme-arg-check-link-9) (push (reg eax)) (assign (reg eax) (op load-array) (label $scmglobal-symbol-Da-Gtstring) (const 0)) (branch-nonzero (label scheme-arg-check-link-10) (reg eax)) (push (const 303)) (perform (op break)) (label scheme-arg-check-link-10) (push (reg eax)) (assign (reg ebx) (op load-array) (label $scmglobal-map) (const 0)) (assign (reg ecx) (op load-array) (reg ebx) (const 1)) (branch-nonzero (label function-check-11) (result (op equal-to) (reg ecx) (const -2147483639))) (perform (op break)) (label function-check-11) (assign (reg ecx) (op load-array) (reg ebx) (const 2)) (assign (reg ebx) (op load-array) (reg ebx) (const 3)) (assign (reg edi) (label scheme-apply-link-12)) (assign (reg edx) (const 2)) (goto (reg ebx)) (label scheme-apply-link-12) (branch-nonzero (label scheme-arg-check-link-13) (reg eax)) (push (const 351)) (perform (op break)) (label scheme-arg-check-link-13) (push (reg eax)) (assign (reg eax) (op load-array) (label $scmglobal-string-Daappend) (const 0)) (branch-nonzero (label scheme-arg-check-link-14) (reg eax)) (push (const 367)) (perform (op break)) (label scheme-arg-check-link-14) (push (reg eax)) (assign (reg ebx) (op load-array) (label $scmglobal-apply) (const 0)) (assign (reg ecx) (op load-array) (reg ebx) (const 1)) (branch-nonzero (label function-check-15) (result (op equal-to) (reg ecx) (const -2147483639))) (perform (op break)) (label function-check-15) (assign (reg ecx) (op load-array) (reg ebx) (const 2)) (assign (reg ebx) (op load-array) (reg ebx) (const 3)) (assign (reg edi) (label scheme-apply-link-16)) (assign (reg edx) (const 2)) (goto (reg ebx)) (label scheme-apply-link-16) (branch-nonzero (label scheme-arg-check-link-17) (reg eax)) (push (const 415)) (perform (op break)) (label scheme-arg-check-link-17) (push (reg eax)) (assign (reg ebx) (op load-array) (label $scmglobal-string-Da-Gtsymbol) (const 0)) (assign (reg ecx) (op load-array) (reg ebx) (const 1)) (branch-nonzero (label function-check-18) (result (op equal-to) (reg ecx) (const -2147483639))) (perform (op break)) (label function-check-18) (assign (reg ecx) (op load-array) (reg ebx) (const 2)) (assign (reg ebx) (op load-array) (reg ebx) (const 3)) (assign (reg edi) (op load-array) (sys frame-pointer) (const -1)) (assign (reg eax) (op add-pointer) (sys stack-pointer) (const 0)) (assign (sys stack-pointer) (sys frame-pointer)) (pop (sys frame-pointer)) (pop (reg edx)) (assign (reg edx) (op bit-lshift) (reg edx) (const 2)) (assign (sys stack-pointer) (op add) (sys stack-pointer) (reg edx)) (assign (reg edx) (op load-array) (reg eax) (const 0)) (push (reg edx)) (assign (reg edx) (const 1)) (goto (reg ebx))))


(define x (sasm-local-max *opt-code*))
(display ";; sasm-local-max-result [")
(display x)
(display "]")
(newline)
