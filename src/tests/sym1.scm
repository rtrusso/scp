(define x (symbol->string 'asdf))
(display (string? x))
(newline)
(write-char (string-ref x 0) (current-output-port))
(write-char (string-ref x 1) (current-output-port))
(write-char (string-ref x 2) (current-output-port))
(write-char (string-ref x 3) (current-output-port))
(newline)
(define y (string->symbol x))
(display y)
(newline)
(display (symbol? y))
(newline)
(display (eq? 'asdf y))
(newline)
