(need scheme/syntax/expander)
(need scheme/transforms/internal-defines)
(need util/rfilact)
(need util/io)
(need util/string)
(need util/list)
(need util/output-file)
(need sasm/sasm-insn)
(need build/rules)
(need scheme/compiler/env)
(need scheme/compiler/context)
(need scheme/compiler/cps)
(need scheme/compiler/settings)
(need scheme/compiler/labels)
(need scheme/compiler/linkage)
(need scheme/compiler/gc)
(need scheme/compiler/codegen)
(need scheme/compiler/codegen/pretty-print)
(need scheme/compiler/codegen/emit)
(need scheme/compiler/codegen/epilogue)
(need scheme/compiler/codegen/immediate)
(need scheme/compiler/codegen/lambda)
(need scheme/compiler/codegen/reference)
(need scheme/compiler/codegen/tailcall)
(need scheme/compiler/codegen/targets)
(need scheme/compiler/codegen/set)
(need scheme/compiler/codegen/application)
(need scheme/compiler/codegen/letrec)

(define *scheme-codegen-version* ";;; scheme-compiler version 1-1-2019 0.1")

(define *schemec-debug* #f)

(define (codegen-entry-chain symbols)
  (define (iter result symbols)
    (if (null? symbols)
        result
        (iter (append-insn-seq (insn-seq '()
                                         `(accum operand index
                                                 ,(cadr (env-register)) ,(cadr (link-register)))
                                         `((perform (op call) (const ,(get-entry-symbol (car symbols))))))
                               result)
              (cdr symbols))))
  (iter (empty-insn-seq) symbols))

(define (compile-scheme-program! code-to-translate main-symbol)
  (define (accum-top-level-defines! statement)
    (if (and (pair? statement)
             (eqv? 'define (car statement))
             (pair? (cdr statement))
             (symbol? (cadr statement))
             (not (member (cadr statement) *top-level-defined-symbols*)))
        (set! *top-level-defined-symbols*
              (cons (cadr statement)
                    *top-level-defined-symbols*))))
  (define (finish-codegen body)
    (define exit-label (make-label (get-entry-symbol main-symbol) (make-label-token)))
    (for-each (lambda (sym)
                (scheme-codegen-emit! (insn-seq '() '()
                                                `((extern ,(get-entry-symbol sym))))))
              *top-level-dependencies*)
    (if (and *emit-entry*
             (top-level-compiler-context?))
        (begin
          ;; (extern $scheme-entry:rtl/r5rs-wrap)
          ;; (extern $scheme-entry:rtl/r5rs-library)
          (scheme-codegen-emit! (insn-seq '() '()
                                          `((extern $scheme-entry:rtl/r5rs-wrap))))
          (scheme-codegen-emit! (insn-seq '() '()
                                          `((extern $scheme-entry:rtl/r5rs-library))))
          ))
    (if (and *emit-entry*
             *conspiracy-mode*)
        (begin
          (scheme-codegen-emit! (insn-seq '() '()
                                          `((export mm-rtl-heap-begin-mark-global-vars)
                                            (export mm-rtl-heap-end-mark-global-vars)
                                            (global mm-rtl-heap-begin-mark-global-vars
                                                    (const 0)
                                                    )
                                            (global mm-rtl-heap-end-mark-global-vars
                                                    (const 0)
                                                    )
                                            )))))
    (scheme-codegen-emit! (insn-seq '() '()
                                    `((export ,(get-entry-symbol main-symbol)))))
    (if (and *emit-entry* (top-level-compiler-context?))
        (scheme-codegen-emit! (insn-seq '() '()
                                        `((entry ,(get-entry-symbol main-symbol))))))
    (scheme-codegen-emit-function!
     (get-entry-symbol main-symbol)
     (append-insn-seq
      (insn-seq '()
                '(accum)
                `((perform (op push-frame))
                  (perform (op reserve-locals) (const 0))
                  ))
      (if (and *emit-entry*
               (top-level-compiler-context?))
          (insn-seq '()
                    `(accum operand index
                            ,(cadr (env-register)) ,(cadr (link-register)))
                    `((perform (op call) (const $scheme-entry:rtl/r5rs-wrap))
                      (perform (op call) (const $scheme-entry:rtl/r5rs-library))))
          (empty-insn-seq))
      (codegen-entry-chain *top-level-dependencies*)
      (insn-seq '() '(accum)
                `((assign (reg accum) (op load-array) (const begin-mark-global-vars-range) (const 0))
                  (branch-nonzero (label ,exit-label) (reg accum))
                  (assign (reg accum) (const begin-mark-global-vars-range))
                  (push (reg accum))
                  (perform (op call) (const cp-rtl-add-global-root-range))))
      (insn-seq '()
                `(,(cadr (env-register)) ,(cadr (link-register)))
                `((assign ,(env-register) (const 0))
                  (assign ,(link-register) (const 0))))
      body
      (with-scheme-linkage (insn-seq '()
                                     '()
                                     `((label ,exit-label)))
                           (return-linkage 0)))
     0))
  (display *scheme-codegen-version* *output*)
  (newline *output*)
  (display (if *expand-only* ";;; expanded from " ";;; compiled from ") *output*)
  (display main-symbol *output*)
  (newline *output*)
  (newline *output*)
  (let ((env (scheme-codegen-environment)))
    (let loop ((code code-to-translate)
               (body (empty-insn-seq)))
      (if (null? code)
          (begin (if (not *expand-only*)
                     (finish-codegen body)))
          (let* ((unused-1 (if *schemec-debug* (begin (display ";; compiling " *output*)
                                                      (write (car code) *output*)
                                                      (newline *output*))))
                 (unused-2 (if *schemec-debug* (begin (display ";; current label token: " *output*)
                                                      (display (make-label-token 'debug) *output*)
                                                      (newline *output*))))
                 (statement (car code))
                 (expanded-statement (car (scheme-expand-macros (list statement) compile-env)))
                 (statement-without-defines (car (rewrite-defines (list expanded-statement))))
                 (finished-statement statement-without-defines))
            (accum-top-level-defines! finished-statement)
            (if *expand-only*
                (begin (pretty-print statement-without-defines *output*)
                       (newline *output*)))
            (loop (cdr code)
                  (preserving (scheme-preserved-registers)
                              body
                              (scheme-codegen finished-statement
                                                     env
                                                     'null
                                                     (next-linkage)))
                  ))))))

;; main ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define *main-file* #f)
(define *output* #f) ; the actual output port
(define *output-stack* '())
(define *input* #f)
(define *input-file* #f)
(define *output-file* #f) ; the output file name
(define *conspiracy-mode* #f)
(define *output-directory* #f)
(define *visited-files* '())

(define (push-output-stack! port)
  (set! *output-stack* (cons *output* *output-stack*))
  (set! *output* port))

(define (pop-output-stack!)
  (close-output-port *output*)
  (set! *output* (car *output-stack*))
  (set! *output-stack* (cdr *output-stack*)))

(define (file-visited? file-name)
  (member file-name *visited-files*))

(define (visit-file! file-name)
  (if (not (file-visited? file-name))
      (set! *visited-files* (cons file-name *visited-files*))))

(define (expand-need exp env ctx)
  (if (and (list? exp)
           (= 2 (length exp))
           (symbol? (car (cdr exp))))
      `(need ,(car (cdr exp)))
      (error "Invalid syntax - malformed need" exp)))

(define (expand-rfilact exp env ctx)
  (if (and (list? exp)
           (= 2 (length exp))
           (string? (car (cdr exp))))
      `(rfilact ,(car (cdr exp)))
      (error "Invalid syntax - malformed read-file-into-list" exp)))

(define (get-output-file-name file-name)
  (let* ((flat (replace-characters-in-string file-name
                                             #\/
                                             #\-))
         (index (string-index-of flat #\.))
         (no-ext (string-prefix flat index)))
    (build-path *output-directory*
                (string-append no-ext
                               (if *expand-only* ".scm" ".sasm")))))

(define (get-output-file-name-tmp file-name)
  (let* ((flat (replace-characters-in-string file-name
                                             #\/
                                             #\-))
         (index (string-index-of flat #\.))
         (no-ext (string-prefix flat index)))
    (build-path *output-directory*
                (string-append "."
                               no-ext
                               (if *expand-only* ".scm" ".sasm")))))

(define (get-syntax-output-file-name file-name)
  (let* ((flat (replace-characters-in-string file-name
                                             #\/
                                             #\-))
         (index (string-index-of flat #\.))
         (no-ext (string-prefix flat index)))
    (build-path *output-directory*
                (string-append no-ext ".sasm-syntax"))))

(define (get-syntax-output-file-name-tmp file-name)
  (let* ((flat (replace-characters-in-string file-name
                                             #\/
                                             #\-))
         (index (string-index-of flat #\.))
         (no-ext (string-prefix flat index)))
    (build-path *output-directory*
                (string-append "." no-ext ".sasm-syntax"))))

(define (get-file-name-need-symbol file-name)
  (string->symbol (string-remove-suffix file-name ".scm")))

(define (compile-need! file-name)
  (if (not (file-visited? file-name))
      (let ((output-file-name (get-output-file-name file-name)))
        (visit-file! file-name)
        (set! *top-level-dependencies*
              (cons (get-file-name-need-symbol file-name)
                    *top-level-dependencies*))
        (let ((in (read-fs file-name))
              (out (read-fs output-file-name)))
          (if (or (not (fs-exists? out))
                  (not (fs-exists? in))
                  (fs-newer? in out)
                  (not (check-output-version *scheme-codegen-version* output-file-name)))
              (compile-file! file-name)
              (begin (display (string-append ";; "
                                             output-file-name
                                             " up-to-date: "
                                             (symbol->string (get-file-name-need-symbol file-name))
                                             ))
                     (newline)
                     (expand-file! file-name)
                     ))))))

(define (expand-file! file-name)
  (let* ((input-file-name file-name)
         (input-file-fs (read-fs input-file-name))
         (syntax-output-file-name (get-syntax-output-file-name file-name))
         (syntax-input-file-name syntax-output-file-name)
         (syntax-input-file-fs (read-fs syntax-input-file-name))
         (syntax-output-file-name-tmp (get-syntax-output-file-name-tmp file-name)))
    (if (and (fs-exists? syntax-input-file-fs)
             (fs-newer? syntax-input-file-fs input-file-fs))

        ;; syntax file up-to-date
        (let ((syntax-input-port (open-input-file syntax-input-file-name)))
          (if (not syntax-input-port)
              (error "Unable to open syntax input file" syntax-input-file-name))
          (display (string-append ";; loading syntax "
                                  syntax-input-file-name))
          (newline)
          (begin-accumulating-define-syntax-expressions!)
          (let ((loaded-syntax (read-fully syntax-input-port)))
            (scheme-expand-macros loaded-syntax compile-env))
          (stop-accumulating-define-syntax-expressions!)
          (close-input-port syntax-input-port)
          (expand-dump-stats))

        ;; regenerate syntax file
        (begin
          (delete-if-exists! syntax-output-file-name-tmp)
          (let ((input-port (open-input-file input-file-name))
                (syntax-output-port (open-output-file syntax-output-file-name-tmp)))
            (if (not input-port)
                (error "Unable to open input file" input-file-name))
            (if (not syntax-output-port)
                (error "Unable to open syntax output file" syntax-output-file-name-tmp))
            (display (string-append ";; compiling syntax "
                                    (symbol->string (get-file-name-need-symbol file-name))
                                    ))
            (newline)
            (begin-accumulating-define-syntax-expressions!)
            (scheme-expand-macros (read-fully input-port) compile-env)
            (stop-accumulating-define-syntax-expressions!)
            (close-input-port input-port)
            (for-each (lambda (x)
                        (pretty-print x syntax-output-port))
                      (get-accumulated-define-syntax-expressions))
            (close-output-port syntax-output-port)
            (rename-temp-file! syntax-output-file-name-tmp syntax-output-file-name)
            (expand-dump-stats)))
        )))

(define (compile-file! file-name)
  (let ((output-file-name (get-output-file-name file-name))
        (output-file-name-tmp (get-output-file-name-tmp file-name))
        (syntax-output-file-name (get-syntax-output-file-name file-name))
        (syntax-output-file-name-tmp (get-syntax-output-file-name-tmp file-name))
        )
    (delete-if-exists! output-file-name-tmp)
    (delete-if-exists! syntax-output-file-name-tmp)
    (display (string-append ";; "
                            output-file-name
                            (if *expand-only*
                                ": expanding "
                                ": compiling ")
                            (symbol->string (get-file-name-need-symbol file-name))
                            ))
    (newline)
    (let* ((input-port (open-input-file file-name))
           (output-port (open-output-file output-file-name-tmp))
           (syntax-output-port (open-output-file syntax-output-file-name-tmp)))
      (if (not input-port)
          (error "Unable to open input file" input-file-name))
      (if (not output-port)
          (error "Unable to open output file" output-file-name-tmp))
      (if (not syntax-output-port)
          (error "Unable to open syntax output file" syntax-output-file-name-tmp))
      (push-output-stack! output-port)
      (push-compiler-context!)
      (begin-accumulating-define-syntax-expressions!)
      (compile-scheme-program! (read-fully input-port)
                               (get-file-name-need-symbol file-name))
      (if (not *expand-only*)
          (scheme-codegen-emit-flush!))
      (close-input-port input-port)
      (for-each (lambda (x)
                  (pretty-print x syntax-output-port))
                (get-accumulated-define-syntax-expressions))
      (close-output-port syntax-output-port)
      (stop-accumulating-define-syntax-expressions!)
      (pop-compiler-context!)
      (pop-output-stack!)
      (rename-temp-file! output-file-name-tmp output-file-name)
      (rename-temp-file! syntax-output-file-name-tmp syntax-output-file-name)
      )))

(let loop ((args (cdr (vector->list *argv*))))
  (define (opt? opt-name str)
    (or (equal? str (string-append "/" opt-name))
        (equal? str (string-append "-" opt-name))))
  (define (long-opt? opt-name str)
    (equal? str (string-append "--" opt-name)))
  (define (looks-like-opt? str)
    (or (starts-with? str "/")
        (starts-with? str "-")))
  (if (not (null? args))
      (let ((arg (car args))
            (rest (cdr args)))
        (cond ((and (not (null? rest))
                    (or (opt? "o" arg)
                        (long-opt? "output" arg)))
               (if *output*
                   (close-output-port *output*))
               (set! *output-file* (car rest))
               (loop (cdr rest)))
              ((long-opt? "no-global-mark" arg)
               (set! *emit-global-marks* #f)
               (loop rest))
              ((long-opt? "no-entry" arg)
               (set! *emit-entry* #f)
               (loop rest))
              ((long-opt? "conspiracy" arg)
               (set! *conspiracy-mode* #t)
               (set! *emit-global-marks* #f)
               (install-special-form 'need expand-need)
               (install-special-form 'read-file-into-list-at-compile-time expand-rfilact)
               (loop rest))
              ((long-opt? "expand-only" arg)
               (set! *expand-only* #t)
               (loop rest))
              ((long-opt? "debug" arg)
               (set! *schemec-debug* #t)
               (loop rest))
              ((and (long-opt? "outdir" arg)
                    (not (null? rest)))
               (set! *output-directory* (car rest))
               (loop (cdr rest)))
              ((looks-like-opt? arg)
               (error "Unrecognized command-line option" arg))
              (else (if *input*
                        (error "more than one input file specified" arg))
                    (set! *input* (open-input-file arg))
                    (if (not *input*)
                        (error "Unable to open input file" arg))
                    (set! *input-file* arg)
                    (set! *main-file* arg)
                    (loop rest))))))

;; (if (and (or *conspiracy-mode*
;;              *output-directory*
;;              (not *emit-entry*))
;;          *expand-only*)
;;     (error "The --expand-only option is mutually exclusive with other options"))

(if (and *conspiracy-mode*
         (not *output-directory*))
    (error "Must specify --outdir <path> with --conspiracy"))

(if (not *input*)
    (error "No input file specified."))

(if *conspiracy-mode*
    (begin (if *output*
               (close-output-port *output*))
           (set! *output* #f)
           (if *input*
               (close-input-port *input*))
           (set! *input* #f)
           (compile-need! *main-file*))
    (let ((output-file-name-tmp (if *output-file*
                                    (string-append *output-file* ".tmp")
                                    #f)))
      (if (and (not *output*) (not *output-file*))
          (set! *output* (current-output-port)))

      (if (and (not *output*)
               *output-file*)
          (begin
            (delete-if-exists! output-file-name-tmp)
            (set! *output* (open-output-file output-file-name-tmp))
            (if (not *output*)
                (error "Unable to open output file " output-file-name-tmp))
            ))

      (if *output-file*
          (let ((fs-input (read-fs *input-file*))
                (fs-output (read-fs *output-file*)))
            (if (and (fs-exists? fs-input)
                     (fs-exists? fs-output)
                     (fs-newer? fs-output fs-input)
                     (check-output-version *scheme-codegen-version* *output-file*))
                (begin
                  (display ";; ")
                  (display *output-file*)
                  (display " up-to-date: ")
                  (display *input-file*)
                  (newline))
                (begin
                  (delete-if-exists! *output-file*)
                  (compile-scheme-program! (read-fully *input*)
                                           'scheme-main)
                  (if (not *expand-only*)
                      (scheme-codegen-emit-flush!))
                  (close-output-port *output*)
                  (set! *output* #f)
                  (rename-temp-file! output-file-name-tmp *output-file*)
                  )
                ))
          (begin
            (compile-scheme-program! (read-fully *input*)
                                     'scheme-main)
            (if (not *expand-only*)
                (scheme-codegen-emit-flush!))))

      (close-input-port *input*)
      (if *output*
          (close-output-port *output*))
      (delete-if-exists! output-file-name-tmp)))
