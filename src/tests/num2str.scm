;; (define (number->string2 z . args)
;;   (define chars "0123456789abcdef")
;;   ;; Note that R5RS defines number->string as a procedure and not a
;;   ;; library procedure.  However right now we provide only a simple
;;   ;; integer implementation as a library procedure.
;;   (define (iter x result radix)
;;     (if (zero? x)
;;         (if (null? result)
;;             (list->string (list #\0))
;;             (list->string result))
;;         (iter (quotient x radix)
;;               (cons (string-ref chars (remainder x radix))
;;                     result)
;;               radix)))

;;   (define (inner x radix)
;;     (if (and (= x -2147483648)
;;              (= radix 10))
;;         "-2147483648"
;;         (let ((s (iter (abs x) '() radix)))
;;           (if (negative? x)
;;               (string-append "-" s)
;;               s))))

;;   (cond ((null? args)
;;          (inner z 10))
;;         ((or (not (null? (cdr args)))
;;              (not (integer? (car args)))
;;              (case (car args)
;;                ((2 8 10 16) #f)
;;                (else #t)))
;;          (error "Invalid radix specified" args))
;;         (else
;;          (inner z (car args)))))

(display -2147483648)
(newline)

(display (abs -2147483648))
(newline)

(display "2147483647->")
(display (number->string 2147483647))
(newline)

(display "-2147483647->")
(display (number->string -2147483647))
(newline)

(display "-2147483648->")
(display (number->string -2147483648))
(newline)
