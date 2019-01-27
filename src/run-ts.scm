;; run-ts.scm
;;
;; Compatibility interface for TinyScheme 1.41. Currently does not
;; work because TinyScheme does not support
;; define-syntax/syntax-rules. I'm just keeping this around for old
;; time's sake. Take a look at needc.scm instead.


(load "compat/tinyscheme.scm")
(load "need.scm")
(need scheme/tag)
(need scheme/genproc)
(define *argv* (list->vector *args*))
(load (vector-ref *argv* 0))
