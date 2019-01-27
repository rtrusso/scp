(need scheme/tag)
(need regex/nfa)
(need regex/dfa)
(need regex/compile)

(define (make-regex-syntax check compiler)
  (tag 'regex-syntax (list check compiler)))
(define (regex-syntax.check syntax)
  (car (contents syntax)))
(define (regex-syntax.compiler syntax)
  (cadr (contents syntax)))

(define *regex-syntax-table* '())
(define (install-regex-syntax! syntax)
  (set! *regex-syntax-table*
        (cons syntax *regex-syntax-table*)))

(define (lookup-regex-syntax rgx)
  (define (iter l)
    (cond ((null? l) #f)
          (((regex-syntax.check (car l)) rgx) (car l))
          (else (iter (cdr l)))))
  (iter *regex-syntax-table*))

(define (lookup-regex-compiler rgx)
  (let ((syn (lookup-regex-syntax rgx)))
    (and syn (regex-syntax.compiler syn))))

(define (compile-subregex rgx)
  (let ((compiler (lookup-regex-compiler rgx)))
    (let ((res (and compiler (compiler rgx))))
      (if (not (nfa? res))
	  (error "Result of compilation should be a NFA -- COMPILE-SUBREGEX"
		 rgx res)
	  res))))

(define (compile-regex rgx)
  (if (procedure? rgx)
      (begin (fast-dfa-reset! rgx)
             rgx)
      (let ((res (compile-subregex rgx)))
        (if (not (nfa? res))
            (error "Reuslt of compilation should be a NFA -- COMPILE-REGEX"
                   rgx res))
        (compile-dfa-to-fast-dfa (nfa->dfa res)))))

(define (define-regex-syntax check compiler)
  (install-regex-syntax!
   (make-regex-syntax check compiler)))

(define (define-regex-keyword-syntax keyword compiler)
  (define-regex-syntax 
    (lambda (rgx) (and (list? rgx) (eqv? keyword (car rgx))))
    (lambda (rgx) (compiler (map compile-subregex (cdr rgx))))))

(define (true? x) (not (eqv? #f x)))

(define (regex-whole-match rgx string)
  (let ((a-dfa (compile-regex rgx)))
    (for-each (lambda (char) (fast-dfa-input! a-dfa char))
              (string->list string))
    (fast-dfa-done? a-dfa)))

(define (test-regex rgx string expected)
  (let ((res (true? (regex-whole-match rgx string))))
    (if (not (equal? expected res))
        (error (format "~ ~ => ~, expected ~" rgx string res expected)))))

(define (regex-match-from regex string idx)
  (let ((a-dfa (compile-regex regex)))
    (define (return idx len)
      (list idx (+ idx len)))
    (define (try-match idx)
      (define (iter cur len)
        (if (>= cur (string-length string))
            (and (fast-dfa-done? a-dfa) (return idx len))
            (let ((pre-done? (fast-dfa-done? a-dfa)))
              (fast-dfa-input! a-dfa (string-ref string cur))
              (cond ((and pre-done? (not (zero? len))
                          (fast-dfa-failed? a-dfa))
                     (return idx len))
                    ((fast-dfa-failed? a-dfa) 
                     (try-match (+ idx 1)))
                    (else 
                     (iter (+ cur 1) (+ len 1)))))))
      (fast-dfa-reset! a-dfa)
      (iter idx 0))
    (try-match 0)))

(define (regex-match regex string)
  (regex-match-from regex string 0))

(define (regex-matching-string regex string)
  (let ((match (regex-match regex string)))
    (and match (substring string (car match) (cadr match)))))

(define (regex-match-complete? regex string)
  (let ((match (regex-matching-string regex string)))
    (and match (string=? string match))))

(define (regex-match-port-line-column regex port line column)
  (let ((a-dfa (compile-regex regex)))
    (define (return res output line column)
      (list (list->string (reverse res)) output line column))
    (define (return-error res char line column)
      (list '*lexer-error*
            (list->string (reverse (cons char res)))
            line
            column))
    (define (next-line line char)
      (if (char=? char #\newline)
          (+ line 1)
          line))
    (define (next-column col char)
      (if (char=? char #\newline)
          0
          (+ col 1)))
    (define (iter res line column)
      (let ((char (peek-char port)))
	(if (eof-object? char)
            (if (fast-dfa-done? a-dfa)
                (return res (fast-dfa-emit a-dfa) line column)
                (return-error res char line column))
	    (let ((pre-done (fast-dfa-done? a-dfa))
                  (pre-done-output (fast-dfa-emit a-dfa)))
	      (fast-dfa-input! a-dfa char)
	      (cond ((and pre-done (not (null? res))
			  (fast-dfa-failed? a-dfa))
		     (return res pre-done-output line column))
		    ((fast-dfa-failed? a-dfa)
                     (return-error res char line column))
		    (else (let ((char (read-char port)))
                            (iter (cons char res)
                                  (next-line line char)
                                  (next-column column char)))))))))
    (iter '() line column)))

(define (regex-match-port regex port)
  (let ((res (regex-match-port-line-column regex port 0 0)))
    (and res (car res))))

(define-regex-syntax char? compile-char)
(define-regex-syntax string? compile-string)
(define-regex-syntax symbol? (lambda (rgx) (compile-string (symbol->string rgx))))
(define-regex-syntax number? (lambda (rgx) (compile-string (number->string rgx))))
(define-regex-syntax (lambda (x)
                       (and (list? x) 
                            (= 2 (length x))
                            (eqv? '@ (list-ref x 0))
                            (string? (list-ref x 1))))
  compile-optional-string)
(define-regex-syntax (lambda (stmt)
                       (and (list? stmt)
                            (= 4 (length stmt))
                            (eqv? ':emit (list-ref stmt 0))))
  (lambda (stmt)
    (compile-emit (compile-subregex (list-ref stmt 3)) ; <- nfa
                  (list-ref stmt 1)                    ; <- value-to-emit
                  (list-ref stmt 2))))                 ; <- emit-priority

(define-regex-keyword-syntax '& compile-append)
(define-regex-keyword-syntax ': compile-or)
;(define-regex-keyword-syntax '@ compile-or)
(define-regex-keyword-syntax '* 
  (lambda (list-of-nfas) (compile-* (compile-append list-of-nfas))))
(define-regex-keyword-syntax '+ 
  (lambda (list-of-nfas) (compile-+ (compile-append list-of-nfas))))
(define-regex-keyword-syntax '^
  (lambda (list-of-nfas) (compile-append (map compile-optional
                                              list-of-nfas))))
(define-regex-keyword-syntax '^&
  (lambda (list-of-nfas) (compile-optional 
                          (compile-append list-of-nfas))))
