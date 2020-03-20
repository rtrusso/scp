;; registers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (sasm-machine-registers)
  *machine-register-list*)

(define (sasm-system-registers)
  *machine-sys-register-list*)

(define (forward-lookup-register reg)
  (if (string? reg)
      (forward-lookup-register (string->symbol reg))
      (let ((entry (assoc reg *machine-register-list*)))
        (if entry
            (string->symbol (cdr entry))
            (let ((arch-entry (member reg (map string->symbol (map cdr *machine-register-list*)))))
              (if arch-entry
                  reg
                  (error "Unable to forward-resolve register " reg)))))))

(define (reverse-lookup-register reg)
  (cond ((symbol? reg)
         (reverse-lookup-register (symbol->string reg)))
        ((string? reg)
         (let ((entry (assoc reg (map cons 
                                      (map cdr *machine-register-list*)
                                      (map car *machine-register-list*)))))
           (if (not entry)
               (error "invalid register name -- reverse-lookup-register" reg))
           (cdr entry)))
        (else
         (error "invalid register spec -- reverse-lookup-register" reg))))

(define (reverse-map-side-effects side-effects)
  (map (lambda (x) 
         (if (and  (list? x)
                   (= 2 (length x))
                   (equal? 'reg (car x)))
             `(reg ,(reverse-lookup-register (cadr x)))
             (error "invalid side-effect statement -- reverse-map-side-effects")))
       side-effects))

