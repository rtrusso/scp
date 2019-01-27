;; smake.scm
;;
;; A make tool.
;;

(need regex/dfa)
(need regex/genlex)
(need smake-lexer)
(need smake-parser)

(let ((start-time (current-seconds)))
  (let ((lex (smake-lexically-analyze (current-input-port))))
    (and lex
         (smake-parse-input lex)
         (let ((end-time (current-seconds)))
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
                             (length lex)
                             " ("
                             (present-number (length lex))
                             ") tokens\n"
                             ";; "
                             (present-ratio (length lex) elapsed-time)
                             " tokens/s\n")))))))
