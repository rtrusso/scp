;; sasm-util.scm
;;
;; A command-line tool that is used to run the sasm analysis engine
;; on sasm source files and produce useful messages to the console.

(need util/list)
(need util/string)
(need util/filesystem)
(need util/io)
(need sasm/sasm-machine)
(need sasm/sasm-eval)
(need sasm/sasm-interp-spec)
(need sasm/sasm-tx)
(need sasm/sasm-ast)
(need sasm/sasm-visitor)
(need sasm/sasm-analyze)
(need sasm/sasm-parse)
(need sasm/sasm-rewrite)
(need sasm/sasm-codegen)

(define *sasm-util-output-port* #f)
(define (sasm-util-output-port)
  (if *sasm-util-output-port*
      *sasm-util-output-port*
      (current-output-port)))

(define (display-symbol-list title symbols)
  (display title (sasm-util-output-port))
  (for-each (lambda (x)
              (display " " (sasm-util-output-port))
              (write x (sasm-util-output-port)))
            symbols)
  (newline (sasm-util-output-port)))

(define (process-file file)
  (let ((code (read-file-fully file)))
    (let ((ast (sasm-parse-program code)))
      (if (not ast)
          (error "SASM parse error in file " file)
          (let ((externs (sasm-program-extern-symbols ast))
                (defined (sasm-program-defined-symbols ast))
                (referenced (sasm-program-referenced-symbols ast)))
            (if (not (sasm-program-analyze-symbols! "main" ast))
                (error "One or more errors encountered"))
            (let* ((rewrite (sasm-rewrite-ast ast))
                   (rewrite-ast (sasm-parse-program rewrite)))
              (display-symbol-list ";; externs:" externs)
              (display-symbol-list ";; defined:" defined)
              (display-symbol-list ";; referenced:" referenced)
              (newline (sasm-util-output-port))
              (newline (sasm-util-output-port))
              (for-each (lambda (code)
                          (sasm-pretty-print code (sasm-util-output-port))
                          (newline (sasm-util-output-port)))
                        rewrite)
              ))))))

(define (command-line args)
  (define (iter files args)
    (cond
     ((null? args)
      (for-each process-file (reverse files)))
     (else
      (let ((arg (car args))
            (rest (cdr args)))
        (cond
         ((starts-with? arg "--out=")
          (set! *sasm-util-output-port*
                (open-output-file (string-strip-prefix arg "--out=")))
          (iter files rest))
         (else
          (iter (cons arg files) rest)))))))
  (iter '() args))

(command-line (cdr (vector->list *argv*)))
