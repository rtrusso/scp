;; get-percents.scm
;; Reads in the specified source code file and displays a list of all
;; unique %-prefixed commands referenced therein.

(define print-percents
  (let ((seen '()))
    (lambda (obj)
      (cond ((and (symbol? obj)
                  (not (member obj seen))
                  (char=? #\% (string-ref (symbol->string obj) 0)))
             (set! seen (cons obj seen))
             (write obj)
             (newline))
            ((pair? obj)
             (print-percents (car obj))
             (print-percents (cdr obj)))))))

(if (not (= (vector-length *argv*) 2))
    (error "usage get-percents <source-file>"))

(let ((file (open-input-file (vector-ref *argv* 1))))
  (let loop ((obj (read file)))
    (if (eof-object? obj)
        'done
        (begin (print-percents obj)
               (loop (read file))))))
