(need util/readhelp)

(define (rfilact-impl . args)
  (read-file-into-list-symbolically args))
