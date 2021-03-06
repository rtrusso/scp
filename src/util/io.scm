(define (read-fully port)
  (define (iter res)
    (let ((obj (read port)))
      (if (eof-object? obj)
          (reverse res)
          (iter (cons obj res)))))
  (iter '()))

(define (read-file-fully filename)
  (call-with-input-file filename read-fully))

(define (println . args)
  (for-each display args)
  (newline))

(define (read-line port)
  (let loop ((s (make-string 4096))
             (n 0)
             (char (read-char port)))
    (cond ((and (eof-object? char) (zero? n))
           char)
          ((or (eof-object? char)
               (char=? char #\newline))
           (substring s 0 n))
          (else
           (if (>= n (string-length s))
               (set! s (string-append s (make-string 4096))))
           (if (char=? (integer->char #x0d) char)
               (loop s n (read-char port))
               (begin
                 (string-set! s n char)
                 (loop s (+ n 1) (read-char port))))))))

(define (read-lines-fully port)
  (let loop ((top '())
             (cur '())
             (line (read-line port)))
    (if (eof-object? line)
        top
        (let ((cell (list line)))
          (if (null? cur)
              (loop cell cell (read-line port))
              (begin (set-cdr! cur cell)
                     (loop top cell (read-line port))))))))

(define (read-backslash-escaped-file-from-port port)
  (let loop ((l (list-builder))
             (line (read-line port)))
    (cond ((eof-object? line)
           (list-finalize! l))
          ((and (> (string-length line) 1)
                (char=? #\\ (string-ref line (- (string-length line) 1))))
           (let ((next (read-line port))
                 (len (string-length line)))
             (if (eof-object? next)
                 (begin (list-build! l (substring line 0 (- len 1)))
                        (list-finalize! l))
                 (loop l (string-append (substring line 0 (- len 1))
                                        next)))))
          (else (list-build! l line)
                (loop l (read-line port))))))
