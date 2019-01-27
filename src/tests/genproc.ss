(declare-genproc foo)
(define-genproc (foo (a <string>) (b <symbol>))
  a)

(define-genproc (foo (a <string>))
  (string-append a "asdf"))

(define-genproc (foo :bar! (a <symbol>))
  (string-append (symbol->string a) ".foo"))

(display (foo "asdf"))
(newline)

(display (foo 'asdf))
(newline)

(display (foo "asdf" 'asdf))
(newline)
