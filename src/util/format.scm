;; util/format.scm
;; printf-style formatting functions

(need util/list)

(define (to-string x)
  (cond ((number? x) (number->string x))
        ((string? x) x)
        ((symbol? x) (symbol->string x))
        ((char? x) (string x))
        ((eqv? #t x) "#t")
        ((eqv? #f x) "#f")
        ((null? x) "()")
        ((list? x) (apply string-append
                          (append (list "(" (to-string (car x)))
                                  (map (lambda (x) (string-append " " (to-string x)))
                                       (cdr x))
                                  (list ")"))))
        ((vector? x) (string-append "#" (to-string (vector->list x))))
        (else "#<unknown>")))

(define (format string . orig-args)
  (define args orig-args)
  (define (next-arg)
    (if (null? args)
        (error "Not enough arguments for the format specifier "
               string
               orig-args))
    (let ((result (car args)))
      (set! args (cdr args))
      result))
  (define (more-args?)
    (not (null? args)))
  (list->string 
   (flatten-tree (map (lambda (char)
                        (cond ((char=? char #\~) 
                               (string->list (to-string (next-arg))))
                              (else char)))
                      (string->list string)))))

(define (fprintln fmt . args)
  (display (apply format fmt args))
  (newline))
