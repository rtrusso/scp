;; r5rs-wrap.scm
;; Definitions of R5RS procedures in terms of %-prefixed intrinsic operations.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; ADVANCED ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (apply proc arg1 ... args)

(define (apply f . args)
  (if (null? (cdr args))
      (%apply f (car args))
      (let loop ((prev args)
                 (curr (cdr args)))
        (if (null? (cdr curr))
            (begin (set-cdr! prev (car curr))
                   (%apply f args))
            (loop curr (cdr curr))))))

;; [[procedure]] (call-with-current-continuation proc)
;; [[procedure]] (call-with-values producer consumer)
;; [[procedure]] (dynamic-wind before thunk after)
;; [[procedure]] (eval expression environment-specifier)
;; [[procedure]] (null-environment version)
;; [[procedure]] (scheme-report-environment version)
;; [[procedure]] (values obj ...)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; CHARACTERS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (char->integer char)
;; [[procedure]] (char<=? char1 char2)
;; [[procedure]] (char<? char1 char2)
;; [[procedure]] (char=? char1 char2)
;; [[procedure]] (char>=? char1 char2)
;; [[procedure]] (char>? char1 char2)
;; [[procedure]] (integer->char n)

(define (char>=? char1 char2)
  (not (char<? char1 char2)))

(define (char>? char1 char2)
  (not (char<=? char1 char2)))

;; (other char procedures implemented in SASM, see r5rs-native.scm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; EQUIVALENCE ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (eq? obj1 obj2)
;; [[procedure]] (eqv? obj1 obj2)

;; implemented in raw SASM, see r5rs-native.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; PAIRS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (car pair)
;; [[procedure]] (cdr pair)
;; [[procedure]] (cons obj1 obj2)
;; [[procedure]] (set-car! pair obj)
;; [[procedure]] (set-cdr! pair obj)

;; implemented in raw SASM, see r5rs-native.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; STRINGS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (make-string k char)
;; [[procedure]] (make-string k)

(define (make-string k . args)
  (if (null? args)
      (%make-string k #\space)
      (%make-string k (car args))))

;; [[procedure]] (string->number string radix)
;; [[procedure]] (string->number string)

(define (string->number string . args)
  (define (helper radix)
    (let ((chars "0123456789abcdef")
          (alt-chars "0123456789ABCDEF"))
      (define (value-of x)
        (let loop ((i 0))
          (cond ((>= i radix)
                 #f)
                ((or (char=? (string-ref chars i) x)
                     (char=? (string-ref alt-chars i) x))
                 i)
                (else
                 (loop (+ i 1))))))
      (define (check-overflow? i result)
        (not (and (= radix 16)
                  (<= i 8))))
      (define (iter idx result)
        (if (= idx (string-length string))
            result
            (if (< result 0)
                #f
                (let ((next (value-of (string-ref string idx))))
                  (if next
                      (let* ((operand (* result radix))
                             (next-result (+ operand next)))
                        (if (and (check-overflow? idx result)
                                 (or (< operand 0)
                                     (< next-result 0)
                                     (< next-result operand))
                                )
                            #f ; overflow
                            (iter (+ idx 1) (+ operand next))
                            )
                        )
                      next)))))
      (if (= (string-length string) 0)
          #f
          (if (char=? (string-ref string 0) #\-)
              (if (= (string-length string) 1)
                  #f
                  (if (and (= radix 10)
                           ; due to parsing as positive, and 32-bit
                           ; limit, INT_MIN is special-cased, since it
                           ; can't be represented directly as a
                           ; positive number
                           (string=? "-2147483648" string))
                      (- -2147483647 1)
                      (let ((res (iter 1 0)))
                        (if res (- 0 res) res))))
              (iter 0 0)))))

  (cond ((= 0 (string-length string))
         #f)
        ((null? args)
         (helper 10))
        (else
         (helper (car args)))))

;; [[procedure]] (string->symbol string)
;; [[procedure]] (string-length string)
;; [[procedure]] (string-ref string k)
;; [[procedure]] (string-set! string k char)

;; implemented in raw SASM, see r5rs-native.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; SYMBOLS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (symbol->string symbol)

;; implemented in raw SASM, see r5rs-native.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; TYPE PREDICATES ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (char? obj)
;; [[procedure]] (input-port? obj)
;; [[procedure]] (number? obj)
;; [[procedure]] (output-port? obj)
;; [[procedure]] (pair? obj)
;; [[procedure]] (procedure? obj)
;; [[procedure]] (string? obj)
;; [[procedure]] (symbol? obj)
;; [[procedure]] (vector? obj)

;; implemented in raw SASM, see r5rs-native.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; VECTORS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (make-vector k fill)
;; [[procedure]] (make-vector k)
;; [[procedure]] (vector-length vector)
;; [[procedure]] (vector-ref vector k)
;; [[procedure]] (vector-set! vector k obj)

(define (make-vector k . args)
  (if (null? args)
      (%make-vector k '())
      (%make-vector k (car args))))

;; implemented in raw SASM, see r5rs-native.scm

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; NUMERICS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (/ z)
;; [[procedure]] (/ z1 z2)
;; [[procedure]] (> x1 x2 x3 ...)
;; [[procedure]] (>= x1 x2 x3 ...)

;; partially implemented in r5rs-native.sasm
;; [[procedure]] (* z1 ...)
;; [[procedure]] (+ z1 ...)
;; [[procedure]] (- z)
;; [[procedure]] (- z1 z2)
;; [[procedure]] (= z1 z2 z3 ...)
;; [[procedure]] (< x1 x2 x3 ...)
;; [[procedure]] (<= x1 x2 x3 ...)

(define (* x . args)
  (cond ((null? args)
         x)
        ((null? (cdr args))
         (%* x (car args)))
        (else
         (let loop ((value x)
                    (rest args))
           (if (null? rest)
               value
               (loop (%* value (car rest))
                     (cdr rest)))))))

(define (+ x . args)
  (cond ((null? args)
         x)
        ((null? (cdr args))
         (%+ x (car args)))
        (else
         (let loop ((value x)
                    (rest args))
           (if (null? rest)
               value
               (loop (%+ value (car rest))
                     (cdr rest)))))))

(define (- x . args)
  (cond ((null? args)
         (%- 0 x))
        ((null? (cdr args))
         (%- x (car args)))
        (else
         (let loop ((value x)
                    (rest args))
           (if (null? rest)
               value
               (loop (%- value (car rest))
                     (cdr rest)))))))

(define (= x y . rest)
  (if (null? rest)
      (%= x y)
      (and (%= x y)
           (let loop ((args rest))
             (if (null? args)
                 #t
                 (and (%= x (car args))
                      (loop (cdr args))))))))

(define (< x y . args)
  (cond ((null? args)
         (%< x y))
        (else
         (and (%< x y)
              (let loop ((current y)
                         (rest args))
                (cond ((null? rest)
                       #t)
                      (else
                       (and (%< current (car rest))
                            (loop (car rest)
                                  (cdr rest))))))))))

(define (<= x y . args)
  (cond ((null? args)
         (%<= x y))
        (else
         (and (%<= x y)
              (let loop ((current y)
                         (rest args))
                (cond ((null? rest)
                       #t)
                      (else
                       (and (%<= current (car rest))
                            (loop (car rest)
                                  (cdr rest))))))))))

;; partial implementations of >, >=

(define (> x y . args)
  (cond ((null? args)
         (not (%<= x y)))
        (else
         (and (not (%<= x y))
              (let loop ((current y)
                         (rest args))
                (cond ((null? rest)
                       #t)
                      (else
                       (and (not (%<= current (car rest)))
                            (loop (car rest)
                                  (cdr rest))))))))))

(define (>= x y . args)
  (cond ((null? args)
         (not (%< x y)))
        (else
         (and (not (%< x y))
              (let loop ((current y)
                         (rest args))
                (cond ((null? rest)
                       #t)
                      (else
                       (and (not (%< current (car rest)))
                            (loop (car rest)
                                  (cdr rest))))))))))

(define (integer? x)
  ;; TODO: number tower
  (number? x))

;; Stub implementations of the functions below are provided assuming
;; the lack of a number tower.  Only integer numbers are supported in
;; these stub implementations.

;; [[procedure]] (ceiling x)
;; [[procedure]] (complex? obj)
;; [[procedure]] (denominator q)
;; [[procedure]] (exact->inexact z)
;; [[procedure]] (exact? z)
;; [[procedure]] (floor x)
;; [[procedure]] (imag-part z)
;; [[procedure]] (inexact->exact z)
;; [[procedure]] (inexact? z)
;; [[procedure]] (integer? obj)
;; [[procedure]] (magnitude z)
;; [[procedure]] (angle z)
;; [[procedure]] (number->string z radix)
;; [[procedure]] (number->string z)
;; [[procedure]] (numerator q)
;; [[procedure]] (rational? obj)
;; [[procedure]] (real-part z)
;; [[procedure]] (real? obj)
;; [[procedure]] (round x)
;; [[procedure]] (truncate x)

(define (exact? x)
  (if (number? x)
      #t
      (error "exact?: expects number")))

(define (inexact? x)
  (if (number? x)
      #f
      (error "inexact?: expects number")))

(define (exact->inexact x)
  (if (number? x)
      x
      (error "exact->inexact: expects number" x)))

(define (inexact->exact x)
  (if (number? x)
      x
      (error "inexact->exact: expects number" x)))

(define (truncate x)
  (if (number? x)
      x
      (error "truncate: expects number" x)))

(define (round x)
  (if (number? x)
      x
      (error "round: expects number" x)))

(define (floor x)
  (if (number? x)
      x
      (error "floor: expects number" x)))

(define (ceiling x)
  (if (number? x)
      x
      (error "ceiling: expects number" x)))

(define (rational? x)
  (number? x))

(define (numerator x)
  (if (number? x)
      x
      (error "numerator: expects number" x)))

(define (denominator x)
  (if (number? x)
      1
      (error "denominator: expects number" x)))

(define (real? x)
  (number? x))

(define (complex? x)
  (number? x))

(define (magnitude x)
  (if (number? x)
      x
      (error "magnitude: expects number" x)))

(define (angle x)
  (if (number? x)
      0
      (error "angle: expects number" x)))

(define (real-part x)
  (if (number? x)
      x
      (error "real-part: expects number" x)))

(define (imag-part x)
  (if (number? x)
      0
      (error "imag-part: expects number" x)))

;; [[procedure]] (acos z)
;; [[procedure]] (asin z)
;; [[procedure]] (atan y x)
;; [[procedure]] (atan z)
;; [[procedure]] (cos z)
;; [[procedure]] (exp z)
;; [[procedure]] (expt z1 z2)
;; [[procedure]] (log z)
;; [[procedure]] (make-polar x3 x)
;; [[procedure]] (make-rectangular x1 x2)
;; [[procedure]] (modulo n1 n2)
;; [[procedure]] (quotient n1 n2)
;; [[procedure]] (remainder n1 n2)
;; [[procedure]] (sin z)
;; [[procedure]] (sqrt z)
;; [[procedure]] (tan z)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; I/O ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; [[procedure]] (close-input-port port)
;; [[procedure]] (close-output-port port)
;; [[procedure]] (char-ready? port)
;; [[procedure]] (char-ready?)
;; [[procedure]] (current-input-port)
;; [[procedure]] (current-output-port)
;; [[procedure]] (eof-object? obj)
;; [[procedure]] (open-input-file filename)
;; [[procedure]] (open-output-file filename)
;; [[procedure]] (peek-char port)
;; [[procedure]] (peek-char)
;; [[procedure]] (read-char port)
;; [[procedure]] (read-char)
;; [[procedure]] (write-char char port)
;; [[procedure]] (write-char char)

;; stub implementation for now

(define (char-ready? . args)
  (if (null? args)
      #t
      (if (input-port? (car args))
          #t
          (error "char-ready?: expects an input-port" (car args)))))

(define (peek-char . args)
  (if (null? args)
      (%peek-char (current-input-port))
      (if (and (input-port? (car args))
               (null? (cdr args)))
          (%peek-char (car args))
          (error "peek-char: invalid arguments" args))))

(define (read-char . args)
  (if (null? args)
      (%read-char (current-input-port))
      (if (and (input-port? (car args))
               (null? (cdr args)))
          (%read-char (car args))
          (error "read-char: invalid arguments" args))))

(define (write-char char . args)
  (if (null? args)
      (%write-char char (current-output-port))
      (%write-char char (car args))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; OTHER ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (error . args)
  (display "ERROR")
  (for-each (lambda (x)
              (display " ")
              (display x))
            args)
  (newline)
  (car 0))
