;; compat/tinyscheme.scm
;;
;; Definitions to make TinyScheme compatible with the project.

;; System interface functions that were added to TinyScheme 1.41 to
;; bootstrap the system:
;;
;;   (setenv <env-var-name> <value>) => #unspecified
;;   (getenv <env-var-name>) => #f | <string>
;;
;;   (delete-file <string>) => #unspecified
;;   (create-directory <string>) => #unspecified
;;
;;   These functions are for measuring elapsed time, and are not based on
;;   any epoch, the values are meaningful only in relative terms.
;;   (current-seconds) => #integer
;;   (current-milliseconds) => #integer
;;
;; Used by build/rules.scm:
;;   (stat <string>) => #f | #(
;;     0-6 - unused
;;     7 - st_size - size in bytes
;;     8 - unused
;;     9 - st_mtime - time of last modification
;;    10-N - unused
;;   )
;;
;; Used for bootstrap:
;;
;;   bitwise-ior
;;   bitwise-and
;;   bitwise-not
;;   arithmetic-shift
;;

;; The current-error-port function is referenced by parse.scm, some of
;; the older code, and may be a mzscheme holdover. It is not defined
;; by r5rs.

(define (current-error-port)
  (current-output-port))

;; == R5RS Stubs ==
;;
;; Stub definitions for functions specified in R5RS but missing from
;; TinyScheme.

;; [[procedure]] (scheme-report-environment version)
;; [[procedure]] (null-environment version)

(define (scheme-report-environment version)
  (error "scheme-report-environment not supported in TinyScheme " version))

(define (null-environment version)
  (error "null-environment not supported in TinyScheme " version))

;; [[procedure]] (numerator q)
;; [[procedure]] (denominator q)

(define (numerator q)
  (error "numerator not supported in TinyScheme " q))

(define (denominator q)
  (error "denominator not supported in TinyScheme" q))

;; [[procedure]] (make-rectangular x1 x2)
;; [[procedure]] (make-polar x3 x)
;; [[procedure]] (real-part z)
;; [[procedure]] (imag-part z)
;; [[procedure]] (magnitude z)
;; [[procedure]] (angle z)

(define (make-rectangular x1 x2)
  (error "make-rectangular not supported in TinyScheme" x1 x2))

(define (make-polar x3 x)
  (error "make-polar not supported in TinyScheme" x3 x))

(define (real-part z)
  (error "real-part not supported in TinyScheme" z))

(define (imag-part z)
  (error "imag-part not supported in TinyScheme" z))

(define (magnitude z)
  (error "magnitude not supported in TinyScheme" z))

(define (angle z)
  (error "angle not supported in TinyScheme" z))

;; == Project Functions ==
;;
;; Following are dependencies on SCM or other interpreters that grew
;; in over time.
;; 
;; (stat) comes from SCM/SLib, and was added in the project's fork of
;; the TinyScheme codebase.

(define (file-exists? x)
  (not (not (stat x))))

;; arithmetic-shift
;; bitwise-and
;; bitwise-ior

(define *argv* (list->vector (cons "scheme-interpreter" *args*)))

(define (read-file-into-list-at-compile-time filename)
  (rfilact-impl filename))
