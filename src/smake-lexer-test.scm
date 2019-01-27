;; smake.scm
;;
;; A make tool.
;;

(need regex/dfa)
(need regex/genlex)
(need smake-lexer)

(let ((start-time (current-seconds)))
  (let ((res (smake-lexically-analyze (current-input-port))))
    (let ((end-time (current-seconds)))
      (if res
          (begin
            (display "(")
            (newline)
            (for-each (lambda (obj)
                        (write obj)
                        (newline))
                      res)
            (display ")")
            (newline)
            (let ((elapsed-time (- end-time start-time)))
              (define (present-number num)
                (number->string (/ (exact->inexact (ceiling (* num 100))) 100)))
              (define (present-ratio numerator denominator)
                (if (zero? denominator)
                    (string-append (number->string numerator)
                                   "/"
                                   (number->string denominator))
                    (present-number (/ numerator denominator))))
              (for-each display
                        (list ";; elapsed time "
                              (present-number elapsed-time)
                              "s\n"
                              ";; "
                              (length res)
                              " ("
                              (present-number (length res))
                              ") tokens\n"
                              ";; "
                              (present-ratio (length res) elapsed-time)
                              " tokens/s\n"))))))))
