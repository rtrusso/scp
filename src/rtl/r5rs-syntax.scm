(define-syntax quasiquote
  (syntax-rules (unquote unquote-splicing)
    ((_ (unquote <obj>))
     <obj>)
    ((_ ((unquote-splicing <obj1>) <obj2> ...))
     (append <obj1>
             (quasiquote (<obj2> ...))))
    ((_ (<obj1> <obj2> ...))
     (cons (quasiquote <obj1>)
           (quasiquote (<obj2> ...))))
    ((_ <obj>)
     (quote <obj>))))

(define-syntax and
  (syntax-rules ()
    ((_)
     #t)
    ((_ <obj>)
     <obj>)
    ((_ <obj1> <obj2> ...)
     (if <obj1>
         (and <obj2> ...)
         #f))))

(define-syntax or
  (syntax-rules ()
    ((_)
     #f)
    ((_ <obj>)
     <obj>)
    ((_ <obj1> <obj2> ...)
     (let ((res <obj1>))
       (if res
           res
           (or <obj2> ...))))))

(define-syntax let
  (syntax-rules ()
    ((let ((name val) ...) body1 body2 ...)
     ((lambda (name ...) body1 body2 ...)
      val ...))
    ((let tag ((name val) ...) body1 body2 ...)
     (letrec ((tag (lambda (name ...)
                     body1 body2 ...)))
       (tag val ...)))))

; Original named-let implementation, seemed slightly sub-optimal:
;    ((let tag ((name val) ...) body1 body2 ...)
;     ((letrec ((tag (lambda (name ...)
;                      body1 body2 ...)))
;        tag)
;      val ...))))

(define-syntax let*
  (syntax-rules ()
    ((let* () body1 body2 ...)
     (let () body1 body2 ...))
    ((let* ((name1 val1) (name2 val2) ...)
       body1 body2 ...)
     (let ((name1 val1))
       (let* ((name2 val2) ...)
         body1 body2 ...)))))

;; LETREC ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 
;; Two versions of letrec macros are given below in comments.  The
;; version from Al Petrofsky is considered more compliant with the
;; wording in R5RS.  Neither is used because neither is convenient
;; for recognition by the optimizer or code generator.  Instead,
;; letrec is considered a special form.

; letrec from Al Petrofsky
;(define-syntax letrec
;  (syntax-rules ()
;    ((_ ((var init) ...) . body)
;     (let ((var 0) ...) ;; todo: switch to 'undefined
;       (let ((var (let ((temp init)) (lambda () (set! var temp))))
;             ...
;             (bod (lambda () . body)))
;         (var) ... (bod))))))

; letrec from R5RS
;(define-syntax letrec
;  (syntax-rules ()
;    ((letrec ((var1 init1) ...) body ...)
;     (letrec "generate_temp_names"
;       (var1 ...)
;       ()
;       ((var1 init1) ...)
;       body ...))
;    ((letrec "generate_temp_names"
;       ()
;       (temp1 ...)
;         ((var1 init1) ...)
;         body ...)
;     (let ((var1 '<undefined>) ...)
;       (let ((temp1 init1) ...)
;         (set! var1 temp1)
;         ...
;         body ...)))
;    ((letrec "generate_temp_names"
;       (x y ...)
;       (temp ...)
;       ((var1 init1) ...)
;       body ...)
;     (letrec "generate_temp_names"
;       (y ...)
;       (newtemp temp ...)
;       ((var1 init1) ...)
;       body ...))))

;(define-syntax begin
;  (syntax-rules ()
;    ((begin)
;     (if #f #f))
;    ((begin exp)
;     exp)
;    ((begin exp1 exp2 ...)
;     (let ((x exp1))
;       (begin exp2 ...)))))

(define-syntax cond
  (syntax-rules (else =>)
    ((cond (else <result1> <result2> ...))
     (begin <result1> <result2> ...))
    ((cond (<test> => <result>) <clause1> <clause2> ...)
     (let ((temp <test>))
       (if temp
           (<result> temp)
           (cond <clause1> <clause2> ...))))
    ((cond (<test>))
     <test>)
    ((cond (<test>) <clause1> <clause2> ...)
     (let ((temp <test>))
       (if temp
           temp
           (cond <clause1> <clause2> ...))))
    ((cond (<test> <result1> <result2> ...))
     (if <test> 
         (begin <result1> <result2> ...)))
    ((cond (<test> <result1> <result2> ...)
           <clause1> <clause2> ...)
     (if <test>
         (begin <result1> <result2> ...)
         (cond <clause1> <clause2> ...)))))

(define-syntax case
  (syntax-rules (else)
;    ((case (key ...)
;       clauses ...)
;     (let ((atom-key (key ...)))
;       (case atom-key clauses ...)))
    ((case key
       (else result1 result2 ...))
     (begin result1 result2 ...))
    ((case key
       ((atoms ...) result1 result2 ...)
       clause clauses ...)
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)
         (case key clause clauses ...)))
    ((case key
       ((atoms ...) result1 result2 ...))
     (if (memv key '(atoms ...))
         (begin result1 result2 ...)))))

(define-syntax do
  (syntax-rules ()
    ((do ((var init step ...) ...)
         (test expr ...)
       command ...)
     (letrec
         ((loop
           (lambda (var ...)
             (if test
                 (begin
                   (if #f #f)
                   expr ...)
                 (begin
                   command
                   ...
                   (loop (do "step" var step ...)
                         ...))))))
       (loop init ...)))
    ((do "step" x)
     x)
    ((do "step" x y)
     y)))

 (define-syntax delay
   (syntax-rules ()
     ((delay <expression>)
      ((lambda (proc)
         (let ((result-ready? #f)
               (result #f))
           (lambda ()
             (if result-ready?
                 result
                 (let ((x (proc)))
                   (if result-ready?
                       result
                       (begin (set! result-ready? #t)
                              (set! result x)
                              result)))))))
       (lambda () <expression>)))))

