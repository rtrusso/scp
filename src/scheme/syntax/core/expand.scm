(define *expand-start* #f)
(define *expand-count* 0)
(define *last-expand-count* 0)
(define *expand-remaining* 0)
(define *expand-reference-count* 0)
(define *expand-immediate-count* 0)
(define *expand-application-count* 0)
(define *expand-specform-count* 0)
(define *expand-macro-count* 0)
(define (expand-dump-stats)
  (set! *last-expand-count* *expand-count*)
  (display ";;  expand stats tot:")
  (display *expand-count*)
  (display " ref:")
  (display *expand-reference-count*)
  (display " imm:")
  (display *expand-immediate-count*)
  (display " app:")
  (display *expand-application-count*)
  (display " mac:")
  (display *expand-macro-count*)
  (display " spc:")
  (display *expand-specform-count*)
  (if (> *expand-remaining* 1)
      (begin (display " rem:")
             (display *expand-remaining*)))
  (if *expand-start*
      (begin
        (display " elp:")
        (display (- (current-milliseconds) *expand-start*))))
  (newline)
  )

;; (define (debug-expand . args)
;;   (if (> *expand-count* 1)
;;       (begin (for-each display args)
;;              (newline))))

(define (expand expression environment context)
;  (debug-expand "expand top-level " expression)
  (if (not *expand-start*)
      (set! *expand-start* (current-milliseconds)))
  (set! *expand-count* (+ 1 *expand-count*))
  (if (>= (- *expand-count* *last-expand-count*)
         100)
      (expand-dump-stats))
  (cond ((immediate? expression)
         (set! *expand-immediate-count* (+ 1 *expand-immediate-count*))
         (expand-immediate expression
                            environment
                            (expand-context2 'context/immediate
                                             `((parent ,context)))))

        ((name? expression)
         ;; (display "expanding name ")
         ;; (write expression)
         ;; (newline)
         ;; (debug-expand "expand reference " expression)
         (set! *expand-reference-count* (+ 1 *expand-reference-count*))
         (let ((result (expand-variable-reference expression
                                                  environment
                                                  (expand-context2 'context/var-reference
                                                                   `((parent ,context))))))
           ;; (debug-expand "expand-variable-reference done " expression)
           ;; (debug-expand "expand-variable-reference result " result)
           result))
        ((pair? expression)
         ;; (display "expanding pair expression: ")
         ;; (write expression)
         ;; (newline)
         (if (name? (car expression))
             (let ((binding (bound-value environment (car expression))))
               (cond ((macro? binding)
                      (set! *expand-macro-count* (+ 1 *expand-macro-count*))
                      ;; (debug-expand "expand macro-app " expression)
                      (expand-macro-application binding
                                                 expression
                                                 environment
                                                 context))
                     ((special-form? binding)
                      (set! *expand-specform-count* (+ 1 *expand-specform-count*))
                      ;; (debug-expand "expand specform " expression)
                      (expand-special-form binding
                                            expression
                                            environment
                                            context))
                     (else
                      ;; (debug-expand "expand app1 " expression)
                      (expand-application
                       expression
                       environment
                       (expand-context2 'context/application
                                        `((parent ,context)))))))
             (begin
               ;; (debug-expand "expand app2 " expression)
               (expand-application expression
                                   environment
                                   (expand-context2 'context/application
                                                    `((parent ,context)))))))
        (else (error "Unable to expand expression: " expression context))))

(define (scheme-expand-macros-to-port code environment output-port)
  (set! *expand-remaining*
        (+ (length code) 1))
  ;; (display "expand-remaining ")
  ;; (display *expand-remaining*)
  ;; (newline)
  (let loop ((remaining-code code))
    (if (not (null? remaining-code))
        (begin (set! *expand-remaining* (- *expand-remaining* 1))
               (let ((result (expand (car remaining-code) environment (top-level-context))))
                 (write result output-port)
                 (newline output-port))
               (loop (cdr remaining-code))))))

(define (scheme-expand-macros code environment)
  ;; (display ";; scheme-expand-macros ")
  ;; (display code)
  ;; (newline)
  (set! *expand-remaining*
        (+ (length code) 1))
  (map (lambda (exp)
         ;; (display ";; scheme-expand-macros map ")
         ;; (display exp)
         ;; (newline)
         (set! *expand-remaining* (- *expand-remaining* 1))
         (let ((map-result (expand exp environment (top-level-context))))
           ;; (display ";; scheme-expand-macros map done ")
           ;; (display exp)
           ;; (display " -> ")
           ;; (display map-result)
           ;; (newline)
           map-result))
       code))

(define (scheme-expand-expression expression environment)
  (expand expression environment (top-level-context)))

(define (expand-application expression environment context)
  ;; (display ";; expand-application ")
  ;; (display expression)
  ;; (newline)
  (set! *expand-application-count* (+ 1 *expand-application-count*))
  (let ((result (cons
                 (expand (car expression)
                         environment
                         (expand-context2 'context/application-op
                                          `((parent ,context))))
                 (map (lambda (exp)
                        (expand exp
                                environment
                                (expand-context2 'context/context
                                                 `((parent ,context)))))
                      (cdr expression)))))
    ;; (display ";; expand-application done")
    ;; (newline)
    result))
