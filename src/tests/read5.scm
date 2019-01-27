(let ((f (open-input-file "tests/read-dat3.scm")))
  (let loop ((obj (read f)))
    (if (eof-object? obj)
        'done
        (begin (display obj)
               (newline)
               (loop (read f))))))
