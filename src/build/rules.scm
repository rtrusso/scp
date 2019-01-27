(need util/string)
(need util/output-file)

(define (path-append prefix . args)
  (let loop ((result (string-append prefix ""))
             (rest args))
    (if (null? rest)
        result
        (let* ((part (car rest))
               (next-result (if (ends-with? result "/")
                                (string-append result part)
                                (string-append result "/" part))))
          (loop next-result (cdr rest))))))

(define (new-exec-result success diagnostic)
  (vector success diagnostic))

(define (exec-result-success? result)
  (vector-ref result 0))

(define (exec-command command)
  (error "exec-command currently unsupported" command))

;; (define (exec-command command)
;;   (display command)
;;   (newline)
;;   (let ((start-time (current-milliseconds)))
;;     (let ((int (system command)))
;;       (let ((stop-time (current-milliseconds)))
;;         (let ((ms-elapsed (- stop-time start-time)))
;;           (display ";; elapsed time ")
;;           (display ms-elapsed)
;;           (display "ms")
;;           (newline)))
;;       (new-exec-result (zero? int) int))))

(define (new-rule outputs inputs commands)
  (vector outputs inputs commands))

(define (rule-outputs rule)
  (vector-ref rule 0))

(define (rule-inputs rule)
  (vector-ref rule 1))

(define (rule-commands rule)
  (vector-ref rule 2))

(define (exec-rule-commands rule)
  (let loop ((commands (rule-commands rule)))
    (if (null? commands)
        (new-exec-result #t 0)
        (let ((result (exec-command (car commands))))
          (if (exec-result-success? result)
              (loop (cdr commands))
              result)))))

(define (check-rule-inputs rule output?)
  (define (inputs-exist? inputs)
    (or (null? inputs)
        (let ((fs (car inputs)))
          (if (not (fs-exists? fs))
              (if output?
                  (begin
                    (display (string-append "don't know how to build input "
                                            (fs-file (car inputs))))
                    (newline)
                    #f)
                  #f)
              (inputs-exist? (cdr inputs))))))

  (let ((input-fs (map read-fs (rule-inputs rule))))
    (inputs-exist? input-fs)))

(define (rule-satisfied? rule)
  (check-rule-inputs rule #f))

(define (build-rule rule)
  (define (output-newer? output inputs)
    (or (null? inputs)
        (let ((newer? (fs-newer? output (car inputs))))
          (if (not newer?)
              (begin (display (string-append ";; input "
                                             (fs-file (car inputs))
                                             " newer than output "
                                             (fs-file output)))
                     (newline)
                     #f)
              (output-newer? output (cdr inputs))))))

  (define (all-outputs-newer? outputs inputs)
    (or (null? outputs)
        (and (output-newer? (car outputs) inputs)
             (all-outputs-newer? (cdr outputs) inputs))))

  (let ((input-fs (map read-fs (rule-inputs rule))))
    (if (not (check-rule-inputs rule #t))
        (error "missing inputs")
        (let ((output-fs (map read-fs (rule-outputs rule))))
          (if (not (all-outputs-newer? output-fs input-fs))
              (exec-rule-commands rule))))))

(define (new-project rules)
  (vector rules))

(define (project-rules project)
  (vector-ref project 0))

(define (build-project project)
  (define (build-rules rules)
    (if (null? rules)
        #t
        (let ((rule (car rules)))
          (if (rule-satisfied? rule)
              (begin (build-rule rule)
                     (build-rules (cdr rules)))
              (begin
                (display "skipping rule inputs not satisfied ")
                (display (rule-inputs rule))
                (newline)
                (build-rules (append (cdr rules) (list rule))))))))
  (build-rules (vector-ref project 0)))

