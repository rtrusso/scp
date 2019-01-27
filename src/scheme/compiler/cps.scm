(need scheme/transforms/cps)

(define (scheme-cps program)
  (if (null? program)
      '()
      (cons (let ((stmt (car program)))
              (if (and (pair? stmt)
                       (eqv? 'define (car stmt)))
                  `(define
                     ,(list-ref stmt 1)
                     ,(cps (list-ref stmt 2) '() '@continue))
                  (cps stmt '() '@continue)))
            (scheme-cps (cdr program)))))

