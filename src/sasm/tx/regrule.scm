;; machine description register rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (register? obj)
  (and (list? obj)
       (member (car obj) '(reg temp sys result))
       (or (and (not (equal? 'reg (car obj)))
                (case (car obj)
                  ((sys label)
                   (and (symbol? (cadr obj))
                        (null? (cddr obj))))
                  ((temp) (and (or (symbol? (cadr obj))
                                   (number? (cadr obj)))
                               (null? (cddr obj))))
                  ((const)
                   (and (null? (cddr obj))
                        (or (number? (cadr obj))
                            (string? (cadr obj))
                            (symbol? (cadr obj)))))
                  (else
                   #t)))
           (or (assoc (cadr obj) *machine-register-list*)
               (if (symbol? (cadr obj))
                   (member (symbol->string (cadr obj)) (map cdr *machine-register-list*))
                   (error "invalid object " obj *last-insn*))))))

(define (register obj)
  (cond ((equal? 'reg (car obj))
         (let ((entry (assoc (cadr obj) *machine-register-list*)))
           (if entry
               (cdr entry)
               (let ((entry2 (member (symbol->string (cadr obj)) (map cdr *machine-register-list*))))
                 (if entry2
                     (car entry2)
                     (error "Invalid register object -- REGISTER" obj))))))
        ((equal? 'sys (car obj))
         (let ((entry (assoc (cadr obj) *machine-sys-register-list*)))
           (if entry
               (cdr entry)
               (let ((entry2 (member (symbol->string (cadr obj)) (map cdr *machine-sys-register-list*))))
                 (if entry2
                     (car entry2)
                     (error "Invalid sys-register object -- REGISTER" obj))))))
        ((equal? 'temp (car obj))
         obj)
        (else (error "Invalid register object -- REGISTER" obj))))

(define (sym-register obj)
  (cond ((equal? 'reg (car obj))
         obj)
        ((equal? 'sys (car obj))
         obj)
        ((equal? 'temp (car obj))
         obj)
        (else (error "Invalid register object -- SYM-REGISTER" obj))))

(define (const-or-reg? x)
  (or (and (equal? 'const (car x))
           (or (label? (cadr x))
               (number? (cadr x))))
      (and (equal? 'label (car x))
           (label? (cadr x)))
      (register? x)))

(define (const-or-reg x)
  (or (and (equal? 'const (car x)) 
           (or (number? (cadr x)) 
               (label? (cadr x)))
           (cadr x))
      (and (equal? 'label (car x))
           (label? (cadr x))
           (cadr x))
      (and (register? x) (register x))))

(define (sym-const-or-reg obj)
  (if (const-or-reg? obj)
      obj
      (error "Invalid register object -- SYM-CONST-OR-REG" obj)))

(define consume-const-or-reg 
  (let ((counter 0))
    (lambda (name)
      (set! counter (+ counter 1))
      `(,(list (lambda (x) (member x '(reg const label))) `(const-or-reg-type ,counter))
        ,(list (lambda (x) (or (symbol? x) (integer? x))) name)))))

(define (rewrite-const-or-reg x)
  (cond ((assoc x *machine-register-list*)
         (cdr (assoc x *machine-register-list*)))
        ((assoc x *machine-sys-register-list*)
         (cdr (assoc x *machine-sys-register-list*)))
        ((label? x)
         (label x))
        ((number? x)
         x)
        (else (error "Unable to rewrite value -- rewrite-const-or-reg" x))))

(define (intconst? x) (integer? x))
(define (intconst x) x)
