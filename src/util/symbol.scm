;;; symbol.scm
;;; Symbol utility functions.

(define (symbol-append arg1 . args)
  (string->symbol
   (apply string-append
          (map symbol->string (cons arg1 args)))))
