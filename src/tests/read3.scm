(define f (open-input-file "tests/read-dat.scm"))
(display (input-port? f))
(newline)
(define c (read f))
(display c)
(newline)
(close-input-port f)
(display 'done)
(newline)