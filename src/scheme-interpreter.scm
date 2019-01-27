(need scheme/meta-eval)
(need util/string)
(need util/io)

(define (copy-file from to)
  (call-with-output-file to
    (lambda (out)
      (call-with-input-file from
        (lambda (in)
          (let loop ((c (read-char in)))
            (if (eof-object? c)
                #t
                (begin (write-char c out)
                       (loop (read-char in))))))))))

(let loop ((environment (meta-eval-r5rs-environment))
           (interpreter-args (cdr (vector->list *argv*)))
           (program-code '())
           (program-args '())
           (output-file-name #f)
           (output-file-port #f)
           (output-file-tmp #f))
  (cond ((null? interpreter-args)
         (begin 
           (meta-eval `(define *argv* ,(list->vector program-args))
                      environment)
           (for-each (lambda (expression)
                       (meta-eval expression environment))
                     program-code)
           (if output-file-port
               (begin (close-output-port output-file-port)
                      (copy-file output-file-tmp output-file-name)
                      (delete-file output-file-tmp)
                      ))
           ))
        ((starts-with? (car interpreter-args)
                       "--run=")
         (let ((program-name (string-strip-prefix (car interpreter-args)
                                                  "--run=")))
           (loop environment
                 (cdr interpreter-args)
                 (append program-code
                         (read-file-fully program-name))
                 (cons program-name program-args)
                 output-file-name
                 output-file-port
                 output-file-tmp)))
        ((starts-with? (car interpreter-args)
                       "--out=")
         (let* ((arg (string-strip-prefix (car interpreter-args)
                                          "--out="))
                (tmp-file (string-append arg ".tmp")))
           (if (or output-file-name
                   output-file-port
                   output-file-tmp)
               (error "--out already specified"))
           (if (file-exists? tmp-file)
               (delete-file tmp-file))
           (let ((output-port (open-output-file tmp-file)))
             (meta-eval `(define (current-output-port)
                           (quote ,output-port))
                        environment)
             (meta-eval `(define (write obj . args)
                           (if (null? args)
                               ((quote ,write) obj (quote ,output-port))
                               (apply (quote ,write) obj args)))
                        environment)
             (meta-eval `(define (display obj . args)
                           (if (null? args)
                               ((quote ,display) obj (quote ,output-port))
                               (apply (quote ,display) obj args)))
                        environment)
             (meta-eval `(define (newline . args)
                           (if (null? args)
                               ((quote ,newline) (quote ,output-port))
                               (apply (quote ,newline) args)))
                        environment)
             (meta-eval `(define (write-char obj . args)
                           (if (null? args)
                               ((quote ,write-char) obj (quote ,output-port))
                               (apply (quote ,write-char) obj args)))
                        environment)
             (loop environment
                   (cdr interpreter-args)
                   program-code
                   program-args
                   arg
                   output-port
                   tmp-file))))
        ((starts-with? (car interpreter-args)
                       "--conspiracy")
         (meta-eval `(define setenv (quote ,setenv))
                    environment)
         (meta-eval `(define getenv (quote ,getenv))
                    environment)
         (meta-eval `(define current-seconds (quote ,current-seconds))
                    environment)
         (meta-eval `(define current-milliseconds (quote ,current-seconds))
                    environment)
         (meta-eval `(define create-directory (quote ,create-directory))
                    environment)
         (meta-eval `(define delete-file (quote ,delete-file))
                    environment)
         (meta-eval `(define file-exists? (quote ,file-exists?))
                    environment)
         (meta-eval `(define error (quote ,error))
                    environment)
         ;; (meta-eval `(define arithmetic-shift (quote ,arithmetic-shift))
         ;;            environment)
         ;; (meta-eval `(define bitwise-ior (quote ,bitwise-ior))
         ;;            environment)
         ;; (meta-eval `(define bitwise-and (quote ,bitwise-and))
         ;;            environment)
         ;; (meta-eval `(define bitwise-not (quote ,bitwise-not))
         ;;            environment)
         (meta-eval '(load "need.scm")
                    environment)
         (meta-eval '(need scheme/tag)
                    environment)
         (meta-eval '(need scheme/genproc)
                    environment)
         (loop environment
               (cdr interpreter-args)
               program-code
               program-args
               output-file-name
               output-file-port
               output-file-tmp))
        ((string=? "--" (car interpreter-args))
         (loop environment
               '()
               program-code
               (append program-args interpreter-args)
               output-file-name
               output-file-port
               output-file-tmp))
        (else
         (error "Invalid command line" interpreter-args))))
