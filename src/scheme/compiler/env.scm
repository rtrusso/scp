;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cxx-bootstrap-syntax
  (read-file-into-list-at-compile-time "rtl/r5rs-syntax.scm"))

(define compile-env
  (let ((env (top-level-env)))
    (scheme-expand-macros cxx-bootstrap-syntax env)
    env))

