(define (bound-variable name id)
  (if (generated-name? name)
      (error "bound-variable called with generated-name" name))
  (list 'bound name id))

(define (bound? binding)
  (and (list? binding)
       (= 3 (length binding))
       (eqv? 'bound (car binding))))

(define generate-unique-id
  (let ((id -1))
    (lambda ()
      (set! id (+ id 1))
      id)))

(define generate-unique-variable-id
  (let ((id 0))
    (lambda ()
      (set! id (+ id 1))
      id)))

(define (generate-name symbol id)
  (string->symbol (string-append "{"
                                 (symbol->string symbol)
                                 " "
                                 (number->string id)
                                 "}")))

(define (generated-name? expression)
  (letrec ((str (and (symbol? expression) (symbol->string expression))))
    (and str
         (string-starts-with? str "{")
         (string-ends-with? str "}")
         (string-contains-char? str #\space))))

(define (generated-name-id genname)
  (letrec ((idxof (lambda (chr) (string-index-of str chr)))
           (str (and (symbol? genname) (symbol->string genname))))
    (and str
         (> (idxof #\space) -1)
         (> (idxof #\}) -1)
         (string->number (substring str
                                    (+ 1 (idxof #\space))
                                    (idxof #\}))))))

(define (generated-name-symbol genname)
  (letrec ((idxof (lambda (chr) (string-index-of str chr)))
           (str (symbol->string genname)))
    (and str
         (> (idxof #\{) -1)
         (> (idxof #\space) -1)
         (string->symbol (substring str
                                    (+ 1 (idxof #\{))
                                    (idxof #\space))))))

(define (bound-value environment expression)
  (if (and (procedure? environment) (name? expression))
      (environment expression)
      (error "Unable to bind value: " environment expression)))

(define (bind-value! environment name value)
  (environment name value))

(define (expand-macro-application macro expression env-of-use context)
  (let* ((uid (generate-unique-id))
         (env-for-expansion (bind-macro-aliases uid macro env-of-use))
         (new-exp (transcribe-macro macro
                                    expression
                                    env-for-expansion
                                    env-of-use
                                    uid)))
    (expand new-exp env-for-expansion context)))

(define (transcribe-macro macro expression env-for-expansion env-of-use uid)
  (let* ((rename (lambda (name)
                   (generate-name name uid)))
         (bound? (lambda (sym)
                   (free? (bound-value env-of-use sym)))))
    ((macro-transcriber macro) expression rename bound?)))

(define (bind-macro-aliases uid macro env-of-use)
  (let ((env-of-definition (macro-env-of-definition macro)))
    (lambda (name . args)
;      (debug-expand "bind-macro-aliases " uid " " name " [" args "]")
      ;; (if (and (generated-name? name)
      ;;          (not (eqv? (generated-name-id name) uid)))
      ;;     (begin (debug-expand " env-of-use " env-of-use)
      ;;            (debug-expand " env value " (bound-value env-of-use name))))
      (define (lookup name)
;      (display "bind-macro: ") (write uid) (write name) (newline)
;      (display "generated? ") (write (generated-name? name))
;      (display " ") (write (generated-name-id name)) (newline)
        (if (and (generated-name? name)
                 (eqv? (generated-name-id name) uid))
            (bound-value env-of-definition (generated-name-symbol name))
            (bound-value env-of-use name)))
      (define (set name value)
        (if (and (generated-name? name)
                 (eqv? (generated-name-id name) uid))
            (bind-value! env-of-use (generated-name-symbol name) value)
            (bind-value! env-of-use name value)))
      (if (null? args)
          (let ((result (lookup name)))
            (if (generated-name? result)
                (begin (display "bind-macro-alias (uid ")
                       (display uid)
                       (display ") resulted in generated name ")
                       (display name)
                       (display " mapped to ")
                       (display result)
                       (error "bind-macro-alias returned generated name" name)))
            result)
          (set name (car args))))))

