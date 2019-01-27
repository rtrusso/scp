(define (x y . args)
  (car args))

(write-char (x #\a #\c))
(write-char (x #\a #\b #\c))


