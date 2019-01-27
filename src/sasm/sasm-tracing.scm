;; sasm-tracing.scm
;;
;; Macros and routines to help trace and profile performance of the
;; SASM codebase.
;;
;; Adapted from util/disable-profiling and util/profile.

(define-syntax profile
  (syntax-rules ()
    ((_ <message> <expression-1> <other-expressions> ...)
     (begin <expression-1> <other-expressions> ...))))

(define *sasm-enable-debug-output* #f)

(define-syntax debug
  (syntax-rules (:display)
    ((_ <message> :display (<expression-1> ...) <return-expression>)
     (begin (if *sasm-enable-debug-output*
                (begin (display ";; " (current-output-port))
                       (display (quote <message>) (current-output-port))
                       (begin (display " " (current-output-port))
                              (display <expression-1> (current-output-port)))
                       ...
                       (newline)))
            <return-expression>))
    ((_ <message> <expression-1> ...)
     (begin (if *sasm-enable-debug-output*
                (begin (display ";; " (current-output-port))
                       (display (quote <message>) (current-output-port))
                       (begin (display " " (current-output-port))
                              (display <expression-1> (current-output-port)))
                       ...
                       (newline)))
            '()))))

;; (need util/format)

;; (define *profile-indent* "")

;; (define (profile-indent)
;;   (set! *profile-indent* (string-append *profile-indent*
;;                                         "  ")))

;; (define (profile-unindent)
;;   (set! *profile-indent* (substring *profile-indent*
;;                                     0
;;                                     (- (string-length *profile-indent*) 2))))

;; (define-syntax profile
;;   (syntax-rules ()
;;     ((_ <message> <expression-1> <other-expressions> ...)
;;      (let* ((start (current-milliseconds)))
;;        (profile-indent)
;;        (let ((value (begin <expression-1> <other-expressions> ...)))
;;          (profile-unindent)
;;          (display *profile-indent*
;;                   (current-error-port))
;;          (display (format <message> (- (current-milliseconds) start))
;;                   (current-error-port))
;;          (newline (current-error-port))
;;          value)))))
