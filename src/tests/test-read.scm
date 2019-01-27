(define o (current-output-port))
(define f (open-input-file "tests/test-read.scm"))
(if (input-port? f)
    (let ((x (read-char f)))
      (display x)
      (newline)
      (close-input-port f))
    (begin (display 9999)
           (newline)))
(write-char #\a o)
