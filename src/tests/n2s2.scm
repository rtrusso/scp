(define (quotient2 numerator denominator)
  (define (sign x)
    (cond ((zero? x)
           0)
          ((negative? x)
           -1)
          (else
           1)))

  (define (iter-fast result remainder denominator depth last-depth)
    ;(display (list 'iter-fast result remainder denominator depth last-depth))
    ;(newline)
    (let ((next (* denominator depth)))
      (if (> next depth) ; this is a multiply overflow
          (if (< next remainder)
              (iter-fast (+ result depth)
                         (- remainder next)
                         denominator
                         next
                         depth)
              (iter-slow result remainder denominator))
          (if (< depth remainder)
              (iter-fast (+ result last-depth) ; depth can't get bigger, so keep iterating at current depth
                         (- remainder depth)
                         denominator
                         depth
                         last-depth)
              (iter-slow result remainder denominator)
              ))))

  (define (iter-slow result remainder denominator)
    (if (< remainder denominator)
        (begin ;(display (list 'iter-slow-result result))
               ;(newline)
               result)
        (iter-fast (+ result 1) (- remainder denominator) denominator 1 0)))

  (let ((result (iter-fast 0 (abs numerator) (abs denominator) 1 0)))
    (if (= (sign numerator) (sign denominator))
        result
        (- 0 result))))

(define (remainder2 numerator denominator)
  (let ((q (quotient2 numerator denominator)))
    (- numerator (* q denominator))))


(define (number->string2 z . args)
  (define (x86-32-bit-intconst x)
    (if (>= x 0)
        (number->string2 x 16)
        (let ((bin (string->list (number->string2 (- x) 2))))
          (define (fix-bin)
            ;; needed to detect overflow when there is no tower
            (if (and (pair? bin) (char=? (car bin) #\-))
                (cdr bin)
                bin))
          ;(display "bin: ")
          ;(display bin)
          ;(newline)
          (let ((fixed-bin (fix-bin)))
            ;(display "fixed-bin: ")
            ;(display fixed-bin)
            ;(newline)
            (let loop ((len (length fixed-bin))
                     (res fixed-bin))
              ;(display "loop len:")
              ;(display len)
              ;(newline)
              (if (< len 32)
                  (loop (+ len 1) (cons #\0 res))
                  (let ((negated (map (lambda (x) (if (char=? x #\0) 1 0)) res)))
                    ;(display "negated: ")
                    ;(display negated)
                    ;(newline)
                    (let ((neg-rev (reverse negated)))
                      (let ((twos-comp
                             (let add-1 ((cur neg-rev))
                               (if (null? cur)
                                   (reverse neg-rev)
                                   (if (= 0 (car cur))
                                       (begin (set-car! cur 1)
                                              (reverse neg-rev))
                                       (begin (set-car! cur 0)
                                              (add-1 (cdr cur))))))))
                        ;(display "twos-comp: ")
                        ;(display twos-comp)
                        ;(newline)
                        (let loop2 ((res '())
                                    (data twos-comp))
                          (if (null? data)
                              (list->string (reverse res))
                              (let* ((x (car data))
                                     (x2 (cdr data))
                                     (y (car x2))
                                     (y2 (cdr x2))
                                     (z (car y2))
                                     (z2 (cdr y2))
                                     (w (car z2))
                                     (w2 (cdr z2)))
                                ;(display "x:")
                                ;(display x)
                                ;(display " y:")
                                ;(display y)
                                ;(display " z:")
                                ;(display z)
                                ;(display " w:")
                                ;(display w)
                                ;(newline)
                                ;(display w2)
                                ;(newline)
                                (loop2
                                 (cons (list-ref '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)
                                                 (+ (* 8 x) (* 4 y) (* 2 z) w))
                                       res)
                                 w2)))))))))))))
  (define chars "0123456789abcdef")
  ;; Note that R5RS defines number->string as a procedure and not a
  ;; library procedure.  However right now we provide only a simple
  ;; integer implementation as a library procedure.
  (define (iter x result radix)
    ;(display (list 'iter x result radix))
    ;(newline)
    (if (zero? x)
        (if (null? result)
            (list->string (list #\0))
            (list->string result))
        (iter (quotient2 x radix)
              (cons (string-ref chars (remainder2 x radix))
                    result)
              radix)))

  (define (inner x radix)
    (if (= x -2147483648)
        (case radix
          ((2) "-10000000000000000000000000000000")
          ((8) "-20000000000")
          ((10) "-2147483648")
          ((16) "-80000000")
          (else (error "number->string - Invalid radix specified" radix)))
        (let ((s (iter (abs x) '() radix)))
          (if (negative? x)
              (string-append "-" s)
              s))))

  (cond ((null? args)
         (inner z 10))
        ((or (not (null? (cdr args)))
             (not (integer? (car args)))
             (case (car args)
               ((2 8 10 16) #f)
               (else #t)))
         (error "number->string Invalid radix specified" args))
        (else
         (if (and (< z 0) (= 16 (car args)))
             (x86-32-bit-intconst z)
             (inner z (car args))))))


(display (number->string2 3123)) ; 3123
(newline)
(display (number->string2 0)) ; 0
(newline)
(display (number->string2 -1)) ; -1
(newline)
(display (number->string2 -3123)) ; -3123
(newline)
(display (number->string2 #x8000ffff 16)) ; 8000ffff (-2147418113)
(newline)
(display (number->string2 #xf000000f 16)) ; f000000f
(newline)
(display (number->string2 -2147483648)) ; -2147483648
(newline)
(display (number->string2 -2147483648 2)) ; -10000000000000000000000000000000
(newline)
(display (number->string2 -2147483648 8)) ; -20000000000
(newline)
(display (number->string2 -2147483648 10)) ; -2147483648
(newline)
(display (number->string2 -2147483648 16)) ; -80000000
(newline)
(display (number->string2 -2147483647)) ; -2147483647
(newline)
(display (number->string2 -2147483647 2)) ; -1111111111111111111111111111111
(newline)
(display (number->string2 -2147483647 8)) ; -17777777777
(newline)
(display (number->string2 -2147483647 10)) ; -2147483647
(newline)
(display (number->string2 -2147483647 16)) ; -7fffffff
(newline)
(display (number->string2 #x8000ffff 10)) ; -2147418113
(newline)
