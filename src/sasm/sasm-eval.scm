;; eval-sa.scm
;; Scheme Assembly Evaluator
;;
;; sasm-eval.scm
;; Slow, dumb explicit evaluator for SASM.  Moved into the main project
;; source tree.

;; SA Instruction Set

;; <register-name> => <symbol>
;; <constant-value> => (| <string> <number> <symbol> ...)
;; <inputi> => (| (reg <register-name>) (const <constant-value>))
;;
;; (assign <register-name> (reg <register-name>))
;; (assign <register-name> (const <constant-value>))
;; (assign <register-name> (op <operation-name>) <input1> ... <inputn>)
;; (assign <register-name> (label <label-name>))
;;
;; (perform (op <operation-name>) <input1> ... <inputn>)
;;
;; (test (op <operation-name>) <input1> ... <inputn>)
;;
;; (branch (label <label-name>))
;; (branch-true (label <label-name>))
;; (branch-false (label <label-name>))
;; (branch-zero (label <label-name>) <input>)
;; (branch-nonzero (label <label-name>) <input>)
;;
;; (goto (label <label-name>))
;; (goto (reg <register-name>))
;;
;; (save <register-name>)
;; (save (reg <register-name>))
;;
;; (restore <register-name>)

(define (eval-sa controller return-register)
  (let ((machine (eval-sa-iter controller controller (make-machine))))
    (machine-read-reg machine return-register)))

(define (eval-sa-iter controller pc machine)
  (define (continue)
    (eval-sa-iter controller (cdr pc) machine))
  (define (code) (car pc))
  (define (instruction) (car (code)))
  (define (continue-after proc)
    (lambda (controller pc machine)
      (proc (car pc) machine)
      (continue)))
  (define (continue-with proc)
    (lambda (controller pc machine)
      (eval-sa-iter controller 
                    (proc controller pc machine)
                    machine)))
  (define (evaluator insn)
    (case insn 
      ((return) (continue-with eval-return))
      ((label) (continue-after (lambda (x y) #t)))
      ((assign) (continue-after eval-assign))
      ((perform) (continue-with eval-perform))
      ((test) (continue-after eval-test))
      ((save) (continue-after eval-save))
      ((restore) (continue-after eval-restore))
      ((push) (continue-after eval-push))
      ((pop) (continue-after eval-pop))
      ((branch branch-true) (continue-with eval-branch))
      ((branch-false) (continue-with eval-branch-false))
      ((branch-zero) (continue-with eval-branch-zero))
      ((branch-nonzero) (continue-with eval-branch-nonzero))
      ((goto) (continue-with eval-goto))))
  (cond 
    ((null? pc) 
     machine)
    ((symbol? (car pc))
     (continue))
    (else
     ((evaluator (instruction)) controller pc machine))))

(define (eval-machine-iter controller machine)
  (define print-trace?
    (let ((env (getenv "SASM_INTERP_TRACE")))
      (lambda () (not (not env)))))
  (define (continue)
    (machine-write-reg machine 'pc (cdr (code)))
    (next))
  (define (code) (machine-read-reg machine 'pc))
  (define (instruction) (car (code)))
  (define (continue-after proc)
    (lambda (controller pc machine)
      (proc (car pc) machine)
      (continue)))
  (define (continue-with proc)
    (lambda (controller pc machine)
      (machine-write-reg machine 'pc (proc controller pc machine))
      (next)))
  (define (evaluator insn)
    (case (car insn)
      ((return) (continue-with eval-return))
      ((label info) (continue-after (lambda (x y) #t)))
      ((assign) (continue-after eval-assign))
      ((perform) (continue-with eval-perform))
      ((test) (continue-after eval-test))
      ((save) (continue-after eval-save))
      ((restore) (continue-after eval-restore))
      ((push) (continue-after eval-push))
      ((pop) (continue-after eval-pop))
      ((branch branch-true) (continue-with eval-branch))
      ((branch-false) (continue-with eval-branch-false))
      ((branch-zero) (continue-with eval-branch-zero))
      ((branch-nonzero) (continue-with eval-branch-nonzero))
      ((goto) (continue-with eval-goto))))
  (define (next)
    (let ((pc (machine-read-reg machine 'pc)))
      (if (and (print-trace?)
               (not (null? pc)))
          (begin (display ">> " ) (display (car pc)) (newline)
                 ))
      (cond 
       ((null? pc) 
        machine)
       ((symbol? (car pc))
        (continue))
       (else
         ((evaluator (instruction)) controller pc machine)))))
  (next))

(define (eval-machine-step controller machine)
  (define print-trace?
    (let ((env (getenv "SASM_INTERP_TRACE")))
      (lambda () (not (not env)))))
  (define (continue)
    (machine-write-reg machine 'pc (cdr (code)))
;    (next)
    )
  (define (code) (machine-read-reg machine 'pc))
  (define (instruction) (car (code)))
  (define (continue-after proc)
    (lambda (controller pc machine)
      (proc (car pc) machine)
      (continue)))
  (define (continue-with proc)
    (lambda (controller pc machine)
      (machine-write-reg machine 'pc (proc controller pc machine))
;      (next)
      ))
  (define (evaluator insn)
    (case (car insn)
      ((return) (continue-with eval-return))
      ((label info) (continue-after (lambda (x y) #t)))
      ((assign) (continue-after eval-assign))
      ((perform) (continue-with eval-perform))
      ((test) (continue-after eval-test))
      ((save) (continue-after eval-save))
      ((restore) (continue-after eval-restore))
      ((push) (continue-after eval-push))
      ((pop) (continue-after eval-pop))
      ((branch branch-true) (continue-with eval-branch))
      ((branch-false) (continue-with eval-branch-false))
      ((branch-zero) (continue-with eval-branch-zero))
      ((branch-nonzero) (continue-with eval-branch-nonzero))
      ((goto) (continue-with eval-goto))))
  (define (next)
    (let ((pc (machine-read-reg machine 'pc)))
      (if (and (print-trace?)
               (not (null? pc)))
          (begin (display ">> " ) (display (car pc)) (newline)
                 ))
      (cond 
       ((null? pc) 
        machine)
       ((symbol? (car pc))
        (continue)
        (next))
       (else
        ((evaluator (instruction)) controller pc machine)))))
  (next))

(define (eval-return controller current-pc machine)
  (let ((code (car current-pc))
        (return-address (machine-pop-arg machine)))
    (if (not (machine-valid-return-address? return-address))
        (error "Invalid return address -- eval-return" return-address)
        (let ((previous-context (machine-return-address-context return-address))
              (previous-pc (machine-return-address-pc return-address)))
          (machine-write-reg machine 'pc previous-pc)
          (machine2-set-current-call-context machine previous-context)
          (cond ((equal? code '(return))
                 #f)
                ((= 2 (length code))
                 (let ((n (eval-input-exp (list-ref code 1) machine)))
                   (let loop ((i n))
                     (if (not (zero? i))
                         (begin (machine-pop-arg machine)
                                (loop (- i 1)))))
                   #f))
                (else
                 (error "Invalid return instruction")))
          previous-pc))))

(define (eval-assign code machine)
  (cond ((or (symbol? (cadr code))
             (and (list? (cadr code))
                  (or (equal? 'sys (list-ref (cadr code) 0))
                      (equal? 'reg (list-ref (cadr code) 0)))))
         (machine-write-reg
          machine
          (if (symbol? (cadr code)) 
              (cadr code)
              (list-ref (cadr code) 1))
          (eval-assign-exp (cddr code) machine)))
        ((equal? 'arg (list-ref (cadr code) 0))
         (machine-set-arg
          machine
          (list-ref (cadr code) 1)
          (eval-assign-exp (cddr code) machine)))
        ((equal? 'local (list-ref (cadr code) 0))
         (machine-set-local
          machine
          (list-ref (cadr code) 1)
          (eval-assign-exp (cddr code) machine)))
        ((equal? 'temp (list-ref (cadr code) 0))
         (machine-set-temp
          machine
          (list-ref (cadr code) 1)
          (eval-assign-exp (cddr code) machine)))
        (else
         (error "Invalid assign statement -- eval-assign" code))))

(define (eval-input-exp code machine)
  (case (car code)
    ((reg) (machine-read-reg machine (cadr code)))
    ((sys) (machine-read-reg machine (cadr code)))
    ((temp) (machine-get-temp machine (cadr code)))
    ((arg) (machine-get-arg machine (cadr code)))
    ((local) (machine-get-local machine (cadr code)))
    ((const) (cadr code))
    ((label) (cadr code))
    ((result) (eval-assign-exp (cdr code) machine))
    ((symconst) (machine-lookup-symconst machine (cadr code)))
    (else (error "Unknown input expression -- EVAL-INPUT-EXP" code machine))))

(define (eval-assign-exp code machine)
  (let ((input (car code)))
    (if (eqv? 'op (car input))
        (perform-operation (cadr input)
                           (map (lambda (c) (eval-input-exp c machine))
                                (cdr code))
                           machine)
        (eval-input-exp input machine))))

(define (eval-perform controller pc machine)
  (let* ((code (car pc))
         (operation (nested-list-ref code 1 1))
         (operands (cddr code)))
    (perform-operation operation
                       (map (lambda (c) (eval-input-exp c machine))
                            operands)
                       machine)
    (case operation
      ((call this-call tail-call)
       (machine-read-reg machine 'pc))
      (else
       (cdr pc)))))

(define *user-defined-operation-implementations* '())

(define (install-operation name impl)
  (cond ((assoc name *user-defined-operation-implementations*)
         =>
         (lambda (p) (set-cdr! p impl)))
        (else (set! *user-defined-operation-implementations*
                    (cons (cons name impl)
                          *user-defined-operation-implementations*)))))

(define (operation-implementation operation-name)
  (case operation-name
    ((+) +)
    ((*) *)
    ((-) -)
    ((/) /)
    ((remainder) remainder)
    ((quotient) quotient)
    ((modulo) modulo)
    ((expt) expt)
    ((sin) sin)
    ((cos) cos)
    ((tan) tan)
    ((asin) sin)
    ((acos) acos)
    ((atan) (lambda (a) (atan a)))
    ((abs) abs)
    ((=) =)
    ((<) <)
    ((>) >)
    ((<=) <=)
    ((>=) >=)
    ((average) (lambda (a b) (/ (+ a b) 2)))
    ((square) (lambda (x) (* x x)))
    (else 
     (cond ((assoc operation-name *user-defined-operation-implementations*)
            =>
            (lambda (p) (cdr p)))
           (else (error "Unable to perform operation " operation-name)
                 #f)))))

(define (perform-operation operation-name args machine)
  (apply (operation-implementation operation-name)
         args))

(define (eval-test code machine)
  (let ((val (eval-assign-exp (cdr code) machine)))
    (machine-write-flag machine val)))

(define (eval-save code machine)
  (let ((reg (let ((reg (list-ref code 1)))
               (if (and (pair? reg) (eqv? 'reg (car reg)))
                   (cadr reg)
                   reg))))
    (machine-push machine (machine-read-reg machine reg))))

(define (eval-push code machine)
  (let ((reg (list-ref code 1)))
    (machine-push-arg machine (eval-input-exp reg machine))))

(define (eval-pop code machine)
  (let ((operand (list-ref code 1)))
    (cond ((equal? 'const (car operand))
           (let ((n (eval-input-exp (list-ref code 1) machine)))
             (let loop ((i n))
               (if (not (zero? i))
                   (begin (machine-pop-arg machine)
                          (loop (- i 1)))))))
          ((member (car operand) '(sys reg))
           (let ((value (machine-pop-arg machine)))
             (machine-write-reg machine (cadr operand) value)))
          (else (error "invalid pop instruction -- eval-pop" code)))))

(define (nested-list-ref list n . args)
  (define (iter l nl)
    (if (null? nl)
        l
        (iter (list-ref l (car nl)) (cdr nl))))
  (iter list (cons n args)))

(define (eval-restore code machine)
  (let ((reg (let ((reg (list-ref code 1)))
               (if (and (pair? reg) (eqv? 'reg (car reg)))
                   (cadr reg)
                   reg))))
    (machine-write-reg
     machine
     reg
     (machine-pop machine))))

(define (eval-branch-with-flag controller pc machine flag)
  (let* ((code (car pc))
         (label (nested-list-ref code 1 1)))
    (cond ((eqv? flag 1)
           (let ((res (member (list 'label label) controller)))
             (if (not res)
                 (error "EVAL-BRANCH: Unable to go to non-existent label " label))
             res))
          ((eqv? flag 0)
           (cdr pc))
          (else
           (machine-dump-state machine)
           (error "EVAL-BRANCH: Invalid value for flag register " flag)))))

(define (eval-branch controller pc machine)
  (eval-branch-with-flag controller pc machine (machine-read-flag machine)))

(define (eval-branch-false controller pc machine)
  (eval-branch-with-flag controller
                         pc
                         machine
                         (let ((flag (machine-read-flag machine)))
                           (cond ((eqv? flag 1) 0)
                                 ((eqv? flag 0) 1)
                                 (else (error "EVAL-BRANCH-FALSE: Invalid value for flag register " flag))))))

(define (eval-branch-zero controller pc machine)
  (let* ((insn (car pc))
         (result (eval-input-exp (list-ref insn 2) machine)))
    ;; (display ";; eval-branch-zero ")
    ;; (display insn)
    ;; (display "; ")
    ;; (display result)
    ;; (newline)
    (if (not (number? result))
        (begin (machine-dump-state machine)
               (error "Invalid result in eval-branch-zero " (car pc) result)))
    (eval-branch-with-flag controller
                           pc
                           machine
                           (if (zero? result) 1 0))))

(define (eval-branch-nonzero controller pc machine)
  (let* ((insn (car pc))
         (result (eval-input-exp (list-ref insn 2) machine)))
    ;; (display ";; eval-branch-zero ")
    ;; (display insn)
    ;; (display "; ")
    ;; (display result)
    ;; (newline)
    (if (not (number? result))
        (error "Invalid result in eval-branch-nonzero " insn result))
    (eval-branch-with-flag controller
                           pc
                           machine
                           (if (zero? result) 0 1))))

(define (eval-goto controller pc machine)
  (let* ((code (car pc))
         (label (eval-input-exp (cadr code) machine)))
;    (display "EVAL-GOTO: ") (display label) (newline)
    (let ((res (member (list 'label label) controller)))
      (if (not res)
          (begin (machine-dump-state machine)
                 (error "EVAL-GOTO: Unable to go to non-existent label " label)))
      res)))

