;;
;; Compiler context variables:
;;
;; These are represented as global variables, but with the capability
;; to be saved and loaded on a stack, for compiling sub-modules.
;;

(define *top-level-defined-symbols* '())
(define *top-level-referenced-symbols* '())
(define *top-level-dependencies* '())
(define *scheme-codegen-emit-delayed* '())
(define *scheme-codegen-emit-delayed-functions* '())

(define (reset-compiler-context!)
  (set! *scheme-codegen-emit-delayed* '())
  (set! *scheme-codegen-emit-delayed-functions* '())
  (set! *top-level-referenced-symbols* '())
  (set! *top-level-defined-symbols* '())
  (set! *top-level-dependencies* '()))

(define *compiler-context-stack* '())

(define (top-level-compiler-context?)
  (= (length *compiler-context-stack*) 1))

(define (push-compiler-context!)
  (let ((saved-state
         (vector *top-level-defined-symbols*
                 *top-level-referenced-symbols*
                 *top-level-dependencies*
                 *scheme-codegen-emit-delayed*
                 *scheme-codegen-emit-delayed-functions*)))
    (set! *compiler-context-stack*
          (cons saved-state
                *compiler-context-stack*))
    (reset-compiler-context!)))

(define (pop-compiler-context!)
  (let ((saved-state (car *compiler-context-stack*)))
    (set! *compiler-context-stack*
          (cdr *compiler-context-stack*))
    (set! *top-level-defined-symbols* (vector-ref saved-state 0))
    (set! *top-level-referenced-symbols* (vector-ref saved-state 1))
    (set! *top-level-dependencies* (vector-ref saved-state 2))
    (set! *scheme-codegen-emit-delayed* (vector-ref saved-state 3))
    (set! *scheme-codegen-emit-delayed-functions* (vector-ref saved-state 4))
    ))

