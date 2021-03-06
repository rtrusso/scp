;; sasm-opt.scm
;; SASM "optimizer" and rewriter

(need sasm/sasm-core)
(need sasm/sasm-dataflow)
(need sasm/sasm-regalloc)
(need sasm/sasm-tracing)
(need sasm/sasm-codegen)
(need util/string)
(need util/output-file)

(define *sasm-opt-version* ";; generated by sasm-opt 12-25-2018 0.1")

(define (sasm-opt port)
  (sasm-display-code (sasm-transform-port port)))

(define (sasm-opt-file fname)
  (sasm-display-code (sasm-transform-file fname)))

(define (sasm-display-code sasm-code)
  (for-each sasm-display-statement sasm-code))

(define (sasm-display-statement sasm-stmt)
  (define (sasm-opt-write x)
    (sasm-pretty-print x (sasm-optimizer-output-port)))
  (define (sasm-opt-display x)
    (display x (sasm-optimizer-output-port)))
  (define (sasm-opt-newline)
    (newline (sasm-optimizer-output-port)))
  (case (car sasm-stmt)
    ((entry global export extern)
     (sasm-opt-write sasm-stmt)
     (sasm-opt-newline)
     (sasm-opt-newline))
    ((define-symconst)
     #f)
    ((class)
     (sasm-opt-display "(class ")
     (sasm-opt-write (cadr sasm-stmt))
     (sasm-opt-newline)
     (for-each (lambda (x)
                 (sasm-opt-display "       ")
                 (sasm-opt-write x)
                 (sasm-opt-newline))
               (cddr sasm-stmt))
     (sasm-opt-display "       )")
     (sasm-opt-newline)
     (sasm-opt-newline))
    ((function)
     (sasm-opt-display "(function")
     (sasm-opt-newline)
     (sasm-opt-display "  ")
     (sasm-opt-write (list-ref sasm-stmt 1))
     (sasm-opt-newline)
     (sasm-opt-display "  ")
     (sasm-opt-write (list-ref sasm-stmt 2))
     (sasm-opt-newline)
     (sasm-opt-display "  (body")
     (sasm-opt-newline)
     ;(sasm-dataflow-annotate (cdr (list-ref sasm-stmt 3)))
     (for-each (lambda (stmt)
                 (sasm-opt-display "   ")
                 (sasm-opt-write stmt)
                 (sasm-opt-newline))
               (cdr (list-ref sasm-stmt 3)))
     (sasm-opt-display "   ))")
     (sasm-opt-newline)
     (sasm-opt-newline))
    (else (error "invalid SASM statement -- sasm-display-statement"))))

(define *sasm-optimizer-output-port* #f)
(define (sasm-optimizer-output-port)
  (if *sasm-optimizer-output-port*
      *sasm-optimizer-output-port*
      (current-output-port)))

(define (sasm-invoke-optimizer *argv*)
  (define sasm-output-file '())
  (define sasm-output-file-tmp '())
  (define sasm-input-file '())
  (define (process-command-line-iter args rest)
    (if (null? args)
        (reverse rest)
        (let ((arg (car args)))
          (cond ((starts-with? arg "--out=")
                 (set! sasm-output-file (string-strip-prefix arg "--out="))
                 (set! sasm-output-file-tmp (string-append sasm-output-file ".tmp"))
                 (delete-if-exists! sasm-output-file-tmp)
                 (set! *sasm-optimizer-output-port*
                       (open-output-file sasm-output-file-tmp))
                 (process-command-line-iter (cdr args) rest))
                ((string=? arg "--progress")
                 (set! *sasm-output-progress* #t)
                 (process-command-line-iter (cdr args) rest))
                ((string=? arg "--debug")
                 (set! *sasm-enable-debug-output* #t)
                 (process-command-line-iter (cdr args) rest))
                ((or (string=? arg "--enable-optimizations")
                     (string=? arg "-O"))
                 (set! *sasm-enable-only-regalloc* #f)
                 (process-command-line-iter (cdr args) rest))
                ((or (string=? arg "--disable-optimizations")
                     (string=? arg "-O-"))
                 (set! *sasm-enable-only-regalloc* #t)
                 (process-command-line-iter (cdr args) rest))
                ((string=? arg "--cheap")
                 (set! *sasm-regalloc-cheap* #t)
                 (process-command-line-iter (cdr args) rest))
                (else
                 (process-command-line-iter (cdr args)
                                            (cons (car args) rest)))))))
  (define (process-command-line args)
    (process-command-line-iter args '()))
  (define (cleanup)
    (if *sasm-optimizer-output-port*
        (begin (close-output-port *sasm-optimizer-output-port*)
               (set! *sasm-optimizer-output-port* #f))
        #f))
  (define (main *argv*)
    (cond ((= 1 (vector-length *argv*))
           (sasm-opt (current-input-port)))
          ((= 2 (vector-length *argv*))
           (set! sasm-input-file (vector-ref *argv* 1))
           (let ((input-fs (read-fs sasm-input-file))
                 (output-fs (read-fs sasm-output-file)))
             (if (and (not (null? sasm-output-file))
                      (fs-exists? input-fs)
                      (fs-exists? output-fs)
                      (fs-newer? output-fs input-fs)
                      (check-output-version *sasm-opt-version* sasm-output-file))
                 (begin (display ";; output file ")
                        (display sasm-output-file)
                        (display " up-to-date: ")
                        (display sasm-input-file)
                        (newline))
                 (begin (delete-if-exists! sasm-output-file)
                        (display *sasm-opt-version* *sasm-optimizer-output-port*)
                        (newline *sasm-optimizer-output-port*)
                        (sasm-opt-file (vector-ref *argv* 1))
                        (cleanup)
                        (rename-temp-file! sasm-output-file-tmp sasm-output-file)))))
          (else
           (error "Invalid command line " *argv*))))
  (begin
    (main (list->vector (process-command-line (vector->list *argv*))))
    (cleanup)))
