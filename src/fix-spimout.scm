(need util/io)
(need util/string)

(define *input-port* (open-input-file (vector-ref *argv* 1)))
(define *output-port* (open-output-file (vector-ref *argv* 2)))

(for-each (lambda (x)
            (define prefix "@SYSTEM-OUT-PRINTLN ")
            (if (string-starts-with? x prefix)
                (begin (display (substring x (string-length prefix) (string-length x))
                                *output-port*)
                       (newline *output-port*))))
          (read-lines-fully *input-port*))

(close-output-port *output-port*)
(close-input-port *input-port*)
