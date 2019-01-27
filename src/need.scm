;; need.scm
;;
;; A bootstrap definition of the need syntax used to import libraries
;; into scheme programs.

(define *conspiracy-root* "./")
(setenv "CONSPIRACY_ROOT" *conspiracy-root*)

(define-syntax need
  (syntax-rules ()
    ((_ sym)
     (need-load (symbol->string 'sym)))))

(define need-load
  (let ((loaded '()))
    (lambda (file)
      (if (not (member file loaded))
          (begin (set! loaded (cons file loaded))
                 (let ((fname (string-append file ".scm")))
                   (if (file-exists? fname)
                       (begin
                         (load fname))
                       (begin
                         (load (string-append *conspiracy-root* fname))))))))))
