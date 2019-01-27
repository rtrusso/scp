(need util/list)

(define (cons-macro transcribe environment)
  (list 'macro transcribe environment))

(define (macro? obj)
  (and (list? obj) (= 3 (length obj)) (eqv? 'macro (car obj))))

(define (macro-transcriber macro)
  (list-ref macro 1))

(define (macro-env-of-definition macro)
  (list-ref macro 2))

(define (lexical-syntax-environment syntax-spec parent-environment)
    (lambda (name . val)
      (define (lookup name)
        (cond ((assoc name syntax-spec) => cdr)
              (else (bound-value parent-environment name))))
      (define (set name val)
	(error "Attempt to set object in lexical syntax environment" name))
      (if (null? val)
          (lookup name)
	  (error "Attempt to mutate lexical syntax environment"
		 name val))))

(define (expand-macro rewrite-rule environment)
  (let* ((literals (list-ref rewrite-rule 1))
	 (rules (cddr rewrite-rule))
	 (patterns (map car rules))
	 (templates (map cadr rules)))
    (cons-macro
     (lambda (expression rename bound?)
       (let loop ((templ templates)
		  (pat patterns))
	 (cond ((null? templ) (error "Bad syntax"
				     'syntax-definition-expression
				     expression))
	       (else (let* ((t (car templ))
			    (p (car pat))
			    (r (syntax-rewrite
				(cdr p)
				t
				literals
				(cdr expression)
				rename)))
		       (if r
			   (car r)
			   (loop (cdr templ)
				 (cdr pat))))))))
     environment)))

;; (let-syntax ((keyword (syntax-rules ...)) ...) <body>)
(define (expand-let-syntax expression environment context)
;  (if (not (free? (bound-value environment 'syntax-rules)))
;      (error "in let-syntax, syntax-rules is bound by a lexical variable"))
  (let* ((syntax-definition-expression expression)
	 (syntax-body (list-ref expression 1))
	 (keywords (map car syntax-body))
	 (rules (map cadr syntax-body)))
    (let ((syntax-env (lexical-syntax-environment (map (lambda (keyword rule)
							 (cons keyword
							       (expand-macro rule environment)))
						       keywords
						       rules)
						  expression)))
      (expand-sequence (cddr expression)
			syntax-env
			context))))

;; (let-syntax ((keyword (syntax-rules ...)) ...) <body>)
(define (expand-letrec-syntax expression environment context)
;  (if (not (free? (bound-value environment 'syntax-rules)))
;      (error "in let-syntax, syntax-rules is bound by a lexical variable"))
  (let* ((syntax-definition-expression expression)
	 (syntax-body (list-ref expression 1))
	 (keywords (map car syntax-body))
	 (rules (map cadr syntax-body)))
    (letrec ((syntax-env (lexical-syntax-environment (map (lambda (keyword rule)
							    (cons keyword
								  (expand-macro rule
										 (lambda args
										   (apply syntax-env args)))))
							  keywords
							  rules)
						     environment)))
      (expand-sequence (cddr expression)
			syntax-env
			context))))

(define *accumulated-define-syntax-expressions-stack* '())
(define *accumulated-define-syntax-expressions* '())
(define *accumulating-define-syntax-expressions* #f)
(define (begin-accumulating-define-syntax-expressions!)
  (set! *accumulated-define-syntax-expressions-stack*
        (cons *accumulated-define-syntax-expressions* *accumulated-define-syntax-expressions-stack*))
  (set! *accumulating-define-syntax-expressions* #t)
  (set! *accumulated-define-syntax-expressions* '()))

(define (stop-accumulating-define-syntax-expressions!)
  (if (null? *accumulated-define-syntax-expressions-stack*)
      (error "stop-accumulating-define-syntax-expressions! not accumulating"))
  (set! *accumulated-define-syntax-expressions*
        (car *accumulated-define-syntax-expressions-stack*))
  (set! *accumulated-define-syntax-expressions-stack*
        (cdr *accumulated-define-syntax-expressions-stack*))
  (set! *accumulating-define-syntax-expressions*
        (not (null? *accumulated-define-syntax-expressions-stack*))))

(define (accumulate-define-syntax-expression! expression)
  (if *accumulating-define-syntax-expressions*
      (set! *accumulated-define-syntax-expressions*
            (cons expression *accumulated-define-syntax-expressions*))))

(define (get-accumulated-define-syntax-expressions)
  (reverse *accumulated-define-syntax-expressions*))

(define (expand-define-syntax expression environment context)
;  (display "expanding define-syntax: ")
;  (write expression)
;  (newline)
  (let (;(syntax-definition-expression expression)
        (keyword (list-ref expression 1))
        (rule (list-ref expression 2)))
    (if (free? (bound-value environment 'syntax-rules))
        (begin
          (bind-value! environment
                       keyword
		       (expand-macro rule environment))
          (accumulate-define-syntax-expression! expression)
          '())
	(error "in define-syntax, syntax-rules was bound by a lexical expression"))))

(define (syntax-rewrite pattern template literals expression rename)
  (let ((match (syntax-pattern-match pattern literals expression)))
;    (display "pattern: ") (write pattern) (newline)
;    (display "template: ") (write template) (newline)
;    (display "literals: ") (write literals) (newline)
;    (display "expression: ") (write expression) (newline)
;    (display "match: ") (write match) (newline)
    (if match
        (syntax-pattern-rewrite template 
                                        (syntax-pattern-variables pattern 
                                                                  literals)
                                        (fold-pattern-environment match)
                                        rename)
        #f)))

(define (syntax-pattern-match pattern literals expression)
  (define (chk-append a b)
    (and a b (append a b)))
  (define (chk-cons a b)
    (and a b (cons (cons '... (cons (syntax-pattern-variables pattern literals)
                                    a)) b)))
  (cond ((symbol? pattern)
         (if (member pattern literals)
             (and (or (eqv? expression pattern)
                      (and (generated-name? expression)
                           (eqv? (generated-name-symbol expression) pattern)))
                  '())
             `((,pattern . ,expression))))
        ((null? expression) (or (and (null? pattern) '())
                                (and (pair? pattern)
                                     (equal? '(...) (cdr pattern))
                                     '())))
        ((pair? pattern)
         (and (pair? expression)
              (if (equal? '(...) (cdr pattern))
                  (chk-cons (syntax-pattern-match (car pattern)
                                                  literals
                                                  (car expression))
                            (syntax-pattern-match pattern
                                                  literals
                                                  (cdr expression)))
                  (chk-append (syntax-pattern-match (car pattern)
                                                    literals
                                                    (car expression))
                              (syntax-pattern-match (cdr pattern)
                                                    literals
                                                    (cdr expression))))))
        (else (and (equal? pattern expression) '()))))

(define (syntax-pattern-variables pattern literals)
  (cond ((pair? pattern)
         (subtract-lists eqv? 
                         (filter symbol?
				 (append (syntax-pattern-variables (car pattern) literals)
					 (syntax-pattern-variables (cdr pattern) literals)))
                         (cons '... literals)))
        ((and (symbol? pattern) (not (eqv? '... pattern)))
         (list pattern))
        (else '())))

(define (fold-pattern-environment environment)
  (let ((elems (filter (lambda (obj)
                         (not (and (pair? obj) (eqv? '... (car obj)))))
                       environment))
        (dotted (filter-map (lambda (obj)
                              (and (pair? obj)
                                   (eqv? '... (car obj))
                                   (let ((used (cadr obj))
                                         (sub (cddr obj)))
                                     (cons used
                                           (fold-pattern-environment sub)))))
                            environment)))
    (define (merge subenv subenvs)
      (define (symbols env)
        (car env))
      (cond ((null? subenvs) (list subenv))
            ((null? (intersect-lists eqv? 
                                     (symbols subenv)
                                     (symbols (car subenvs))))
             (cons (cons (append (symbols (car subenvs)) (symbols subenv))
                         (append (cdar subenvs) (cdr subenv)))
                   (cdr subenvs)))
            (else (cons (car subenvs) (merge subenv (cdr subenvs))))))
    (define (bucketize res subenvs)
      (if (null? subenvs)
          res
          (bucketize (merge (car subenvs) res) (cdr subenvs))))
    (append elems
            (map (lambda (a) (cons '... a))
                 (bucketize '() dotted)))))

(define (syntax-pattern-rewrite template syntax-variables environment rename)
  (define used syntax-variables)
  (define (pattern-variables template)
    (intersect-lists eqv?
                     (syntax-pattern-variables template '())
                     used))
  (define (fix obj) (cons obj '()))
  (define (unfix obj) (and obj (car obj)))
  (define (chk-app a b)
    (and (all? (lambda (x) x) a)
         b
         (fix (append (map unfix a) (unfix b)))))
  (define (chk-con a b)
    (and a b (fix (cons (unfix a) (unfix b)))))
  (define (suitable-subenv? template subenv)
    (and (eqv? '... (car subenv))
         (list-subset? eqv? 
                       (pattern-variables template)
                       (cadr subenv))
         (not (null? (pattern-variables template)))
         (cddr subenv)))
  (define (iter template environment rename)
    (cond ((pair? template)
           (if (and (pair? (cdr template)) (eqv? '... (cadr template)))
               (chk-app (map (lambda (subenv)
                               (iter (car template)
                                     subenv
                                     rename))
                             (filter-map (lambda (subenv)
                                           (suitable-subenv? (car template)
                                                             subenv))
                                         environment))
                        (iter (cddr template)
                              environment
                              rename))
               (chk-con (iter (car template) 
                              environment
                              rename)
                        (iter (cdr template)
                              environment
                              rename))))
          ((null? template) (fix template))
          ((symbol? template)
           (if (assoc template environment)
               (fix (cdr (assoc template environment)))
               (and (not (member template used)) (fix (rename template)))))
          (else (fix template))))
  (iter template environment rename))

;; (display "installing syntax forms into the expander")
;; (newline)
(install-special-form 'define-syntax expand-define-syntax)
(install-special-form 'let-syntax    expand-let-syntax)
(install-special-form 'letrec-syntax expand-letrec-syntax)
;; (display "installed syntax forms into the expander")
;; (newline)
