;;; scheme-compiler version 04-21-2018 0.1
;;; expanded from scheme-main

(define read2
  (lambda (bound_00000002@port)
    (letrec ((bound_00000055@read-string (lambda (bound_00000086@res)
                                           (display "read-string")
                                           (newline)
                                           ((lambda (bound_00000088@char)
                                              (if (eof-object? bound_00000088@char)
                                                  (list->string (reverse bound_00000086@res))
                                                  (if (char=? #\\ bound_00000088@char)
                                                      (bound_00000055@read-string (cons (read-char bound_00000002@port)
                                                                                        bound_00000086@res))
                                                      (if (char=? #\" bound_00000088@char)
                                                          (list->string (reverse bound_00000086@res))
                                                          (bound_00000055@read-string (cons bound_00000088@char bound_00000086@res))))))
                                            (read-char bound_00000002@port))))
             (bound_00000031@read-list (lambda (bound_00000069@l)
                                         (display "read-list")
                                         (newline)
                                         (bound_00000018@eat-whitespace)
                                         (if (eof-object? (begin (display "before rl-peek-char")
                                                                 (newline)
                                                                 ((lambda (bound_00000078@r)
                                                                    (display "1after peek-char-")
                                                                    (newline)
                                                                    (display bound_00000078@r)
                                                                    (newline) bound_00000078@r)
                                                                  (peek-char bound_00000002@port))))
                                             (error "Input was terminated while looking for a closing parens.")
                                             (if ((lambda (bound_00000079@r2) (display "r2") (newline) (display bound_00000079@r2) (newline) bound_00000079@r2) (char=? (begin (display "2nd peek-char") (newline) ((lambda (bound_00000081@r) (display "2ndafter peek-char============") (newline) (display bound_00000081@r) (newline) bound_00000081@r) (peek-char bound_00000002@port))) #\)))
                                                 (begin (display "read close paren") (newline) (read-char bound_00000002@port) (reverse bound_00000069@l))
                                                 (begin (display "read-list else") (newline) ((lambda (bound_00000084@elem)
                                                                                                (if (if (symbol? bound_00000084@elem)
                                                                                                        (string=? "." (symbol->string bound_00000084@elem))
                                                                                                        #f)
                                                                                                    ((lambda (bound_00000085@ans)
                                                                                                       (bound_00000018@eat-whitespace)
                                                                                                       (bound_00000007@eat #\)) bound_00000085@ans) (bound_00000024@fix-improper bound_00000069@l (read2 bound_00000002@port))) (bound_00000031@read-list (cons bound_00000084@elem bound_00000069@l)))) (read2 bound_00000002@port)))))))
             (bound_00000024@fix-improper (lambda (bound_00000067@head bound_00000068@tail) (display "fix-improper") (newline) (if (null? bound_00000067@head) bound_00000068@tail (bound_00000024@fix-improper (cdr bound_00000067@head) (cons (car bound_00000067@head) bound_00000068@tail)))))
             (bound_00000018@eat-whitespace (lambda () (display "eat-whitespace") (display bound_00000002@port) (newline) (if (begin (display "before char-whitespace ") (newline) ((lambda (bound_00000066@v) (display "after char-whitespace ") (newline) (display bound_00000066@v) (newline) bound_00000066@v) (char-whitespace? (peek-char bound_00000002@port)))) (begin (display "before read-char ") (newline) (read-char bound_00000002@port) (display "after read-char") (newline) (bound_00000018@eat-whitespace)) '())))
             (bound_00000007@eat (lambda (bound_00000061@char) (display "eat") (newline) (if ((lambda (bound_00000063@res) (if bound_00000063@res bound_00000063@res (member (read-char bound_00000002@port) bound_00000061@char))) (if (char? bound_00000061@char) (char=? bound_00000061@char (read-char bound_00000002@port)) #f)) #t (error "Expecting other character -- READ" bound_00000002@port bound_00000061@char))))
             (bound_00000003@standard-delimiters (list->string (list #\space #\( #\) #\newline #\" #\; #\" (integer->char 13) (integer->char 9)))))
      (display "read2 let*")
      (newline)
      ((lambda (bound_00000089@char)
         ((lambda (bound_00000090@next)
            ((lambda () (if (eof-object? bound_00000089@char) bound_00000089@char (if (char=? #\" bound_00000089@char) (bound_00000055@read-string '()) (if (char=? #\( bound_00000089@char) (bound_00000031@read-list '()) bound_00000089@char))))))
          (if (eof-object? bound_00000089@char)
              bound_00000089@char
              (peek-char bound_00000002@port))))
       (read-char bound_00000002@port)))))

(define f (open-input-file "tests/read-dat.scm"))
(display f)
(newline)
(define c (read2 f))
(display c)
(newline)
(close-input-port f)
