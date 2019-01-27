;; labels ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (label? l)
  (symbol? l))

(define *sasm-backend-specific-label-transform* #f)
(define (label l)
  (if *sasm-backend-specific-label-transform*
      (*sasm-backend-specific-label-transform* l)
      (string-append "_" 
                     (escape-string (lambda (c)
                                      (case c
                                        ((#\-) '(#\_))
                                        ((#\_) '(#\_))
                                        ((#\/) '(#\_ #\_ #\S #\l))
                                        ((#\?) '(#\_ #\_ #\Q #\u))
                                        ((#\%) '(#\_ #\_ #\P #\e))
                                        ((#\>) '(#\_ #\_ #\G #\t))
                                        ((#\<) '(#\_ #\_ #\L #\t))
                                        ((#\=) '(#\_ #\_ #\E #\q))
                                        ((#\!) '(#\_ #\_ #\B #\a))
                                        ((#\*) '(#\_ #\_ #\S #\t))
                                        ((#\+) '(#\_ #\_ #\P #\l))
                                        ((#\@) '(#\_ #\_ #\A #\t))
                                        ((#\$) '(#\_ #\_ #\D #\o))
                                        ((#\\) '(#\_ #\_ #\B #\s))
                                        ((#\:) '(#\_ #\_ #\C #\o))
                                        (else (list c))))
                                    (symbol->string l)))))

