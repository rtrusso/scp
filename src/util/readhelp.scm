(need util/io)
(need util/filesystem)

(define (read-file-into-list-symbolically l)
  (define (resolve-filename l)
    (define (cur s)
      (cond ((string? s)
             (cond ((starts-with? s "$")
                    (let ((val (getenv (substring s 1 (string-length s)))))
                      (if val val
                          (error "Unable to resolve environment variable in read-file-into-list-at-compile-time directive: " s))))
                   (else s)))
            (else #f)))
    (cond ((null? l) #f)
          ((null? (cdr l)) (cur (car l)))
          (else (build-path (cur (car l))
                            (resolve-filename (cdr l))))))
  (read-file-fully (resolve-filename l)))

