(define (read-fs file)
  (let ((s (stat file)))
    (cons s file)))

(define (fs-exists? fs)
  (and fs (pair? fs) (car fs)))

(define (fs-mod-time fs)
  (and (fs-exists? fs)
       (vector-ref (car fs) 9)))

(define (fs-newer? fs-a fs-b)
  (and (fs-exists? fs-a)
       (fs-exists? fs-b)
       (>= (fs-mod-time fs-a)
           (fs-mod-time fs-b))))

(define (fs-file fs)
  (cdr fs))

(define (fs-size-bytes fs)
  (and (fs-exists? fs)
       (vector-ref (car fs) 7)))

(define (delete-if-exists! file-name)
  (if (fs-exists? (read-fs file-name))
      (begin
        (display ";; deleting ")
        (display file-name)
        (newline)
        (delete-file file-name))))

(define (rename-temp-file! file-name-tmp file-name)
  (delete-if-exists! file-name)
  (display ";; renaming ")
  (display file-name-tmp)
  (display " to ")
  (display file-name)
  (newline)
  (rename-file file-name-tmp file-name))

(define (check-output-version version-string output-file)
  (call-with-input-file output-file
    (lambda (port)
      (let loop ((index 0))
        (let ((c (read-char port)))
          (cond ((eof-object? c) #f)
                ((and (char=? c #\newline)
                      (= index (string-length version-string)))
                 #t)
                ((>= index (string-length version-string)) #f)
                ((not (char=? (string-ref version-string index) c))
                 #f)
                (else (loop (+ index 1)))))))))
