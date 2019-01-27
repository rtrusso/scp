(need parse/parse)
(load "ast.scm")
(load "pretty.scm")
(load "minijava-lexer.scm")
(load "minijava-parser.scm")

(let ((parse (minijava-parse-input (call-with-input-file (vector-ref *argv* 1) minijava-lexically-analyze))))
  (and parse
       (display "ok, parsed file")
       (newline)
       (accept-visitor parse pretty-print-ast 0)))
