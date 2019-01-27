;; pat.scm
;;
;; Some simple s-exp pattern matching and rewriting routines.

(define (pattern-match pattern obj)
  (define (terminal? pat)
    (or (null? pat)
        (number? pat)
        (boolean? pat)
        (symbol? pat)))
  (define (combine-result a b)
    (and a b (append a b)))
  (define (test-list results)
    (define (iter current-result remaining-results)
      (if (null? remaining-results)
          current-result
          (iter (and current-result
                     (car remaining-results))
                (cdr remaining-results))))
    (iter #t results))

  (define (accumulate-environment env obj sym)
    ;;
    ;; Checks to see if the given symbol is already defined in the
    ;; environment, and if so, validates that it is already defined to
    ;; an equal object.  If not, it is added to the environment.
    ;;
    (let ((entry (assoc sym env)))
      (cond ((not entry)
             (cons (cons sym obj)
                   env))
            ((equal? (cdr entry) obj)
             env)
            (else
             #f))))
  (define (match-in-env env pat obj)
    (cond ((terminal? pat)
           (if (equal? pat obj)
               env
               #f))

          ;;
          ;; (? <symbol>)
          ;;
          ;; Matches anything.
          ;;
          ((and (pair? pat)
                (eqv? '? (car pat)))
           (and (pair? (cdr pat))
                (null? (cddr pat))
                (symbol? (cadr pat))
                (accumulate-environment env obj (cadr pat))))

          ;;
          ;; (<predicate> <symbol>)
          ;;
          ;; Matches any object that matches the specified predicate.
          ;;
          ((and (pair? pat)
                (procedure? (car pat)))
           (and (pair? (cdr pat))
                (null? (cddr pat))
                (symbol? (cadr pat))
                ((car pat) obj)
                (accumulate-environment env obj (cadr pat))))

          ;;
          ;; (?? <predicate-transform> <symbol>)
          ;;
          ;; Matches an object that matches the specified
          ;; <predicate-transform>.  To build the result, the return
          ;; value of <predicate-transform> applied to that object is
          ;; used.
          ;;
          ((and (pair? pat)
                (eqv? '?? (car pat))
                (pair? (cdr pat))
                (procedure? (cadr pat))
                (pair? (cddr pat))
                (symbol? (caddr pat))
                (null? (cdddr pat)))
           (let ((sym (caddr pat))
                 (predicate (cadr pat)))
             (let ((transformed-obj (predicate obj)))
               (and transformed-obj
                    (accumulate-environment env transformed-obj sym)))))

          ;;
          ;; (@ <predicate> <symbol>)
          ;;
          ;; Matches a list of elements where each element matches
          ;; <predicate>.
          ;;
          ((and (pair? pat)
                (eqv? '@ (car pat))
                (pair? (cdr pat))
                (procedure? (cadr pat))
                (pair? (cddr pat))
                (symbol? (caddr pat))
                (null? (cdddr pat)))
           (let ((sym (caddr pat))
                 (predicate (cadr pat)))
             (and (list? obj)
                  (test-list (map predicate obj))
                  (accumulate-environment env obj sym))))

          ;;
          ;; (@ <predicate> <transform> <symbol>)
          ;;
          ;; Matches a list of elements where each element matches
          ;; <predicate>.  To build the result, <transform> is invoked
          ;; on each element of the list.
          ;;
          ((and (pair? pat)
                (eqv? '@ (car pat))
                (pair? (cdr pat))
                (procedure? (cadr pat))
                (pair? (cddr pat))
                (procedure? (caddr pat))
                (pair? (cdddr pat))
                (symbol? (cadddr pat))
                (null? (cddddr pat)))
           (let ((sym (cadddr pat))
                 (predicate (cadr pat))
                 (transform (caddr pat)))
             (and (list? obj)
                  (test-list (map predicate obj))
                  (accumulate-environment env (map transform obj) sym))))

          ;;
          ;; (@@ <predicate-transform> <symbol>)
          ;;
          ;; Matches a list of elements where each element matches
          ;; <predicate-transform>.  To build the result list, the
          ;; return value of <predicate-transform> applied to that
          ;; element is used.
          ;;
          ((and (pair? pat)
                (eqv? '@@ (car pat))
                (pair? (cdr pat))
                (procedure? (cadr pat))
                (pair? (cddr pat))
                (symbol? (caddr pat))
                (null? (cdddr pat)))
           (let ((sym (caddr pat))
                 (predicate-transform (cadr pat)))
             (and (list? obj)
                  (let ((transformed-obj (map predicate-transform obj)))
                    (and (test-list transformed-obj)
                         (accumulate-environment env transformed-obj sym))))))

          ;;
          ;; Recursive case - invoke recursively on the car and cdr
          ;;
          ((pair? pat)
           (and (pair? obj)
                (let ((new-env (match-in-env env (car pat) (car obj))))
                  (and new-env
                       (match-in-env new-env (cdr pat) (cdr obj))))))
          (else (error "Invalid pattern -- " pat))))
  (match-in-env '() pattern obj))

(define (pattern-match-replace pattern spec)
  (define (terminal? pat)
    (or (null? pat)
        (number? pat)
        (boolean? pat)
        (symbol? pat)))
  (define (combine-res a b)
    (and a b (append a b)))
  (define (replace-in-env obj)
    (cond ((terminal? obj)
           obj)
          ((and (pair? obj)
                (or (eqv? '? (car obj))
                    (procedure? (car obj))))
           (let ((entry (assoc (cadr obj) spec)))
             (if entry 
                 (cdr entry) 
                 (error "Bad match spec for pattern -- pattern-match-replace"))))
          ((pair? obj)
           (cons (replace-in-env (car obj))
                 (replace-in-env (cdr obj))))
          (else (error "Bad pattern -- pattern-match-replace"))))
  (replace-in-env pattern))
