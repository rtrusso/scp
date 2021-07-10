;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define cxx-bootstrap-syntax
  (read-file-into-list-at-compile-time "rtl/r5rs-syntax.scm"))

(define get-compile-env
  (let ((cached-env #f))
    (lambda ()
      (if (not cached-env)
          (begin (set! cached-env (top-level-env))
                 (scheme-expand-macros cxx-bootstrap-syntax cached-env)))
      cached-env)))

