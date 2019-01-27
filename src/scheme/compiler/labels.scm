;; scheme/compiler/labels.scm
;;
;; Utility routines for remapping scheme symbols to sasm labels.

(define (get-entry-symbol need-symbol)
  (string->symbol (string-append "$scheme-entry:"
                                 (symbol->string need-symbol))))

(define (make-sasm-label-for-global global-sym)
  (define (remap-character c)
    (case c
      ((#\-) "-Da")
      ((#\_) "-Us")
      ((#\\) "-Bs")
      ((#\<) "-Lt")
      ((#\>) "-Gt")
      ((#\=) "-Eq")
      ((#\!) "-Ex")
      ((#\@) "-At")
      ((#\$) "-Do")
      ((#\%) "-Pt")
      ((#\^) "-Ca")
      ((#\&) "-Am")
      ((#\*) "-As")
      ((#\+) "-Pl")
      ((#\|) "-Pi")
      ((#\:) "-Co")
      ((#\.) "-Pr")
      ((#\/) "-Fs")
      (else (string c))))
  (define (remap-symbol sym)
    (apply string-append
           (map remap-character
                (string->list (symbol->string sym)))))
  (string->symbol (string-append "$scmglobal-"
                                 (remap-symbol global-sym))))

(define (make-label-for-literal)
  (make-label 'scheme-literal (make-label-token)))


(define (make-label-for-global global-sym)
  (make-sasm-label-for-global global-sym))

(define (make-label-for-lambda-with-token token)
  (make-label 'scheme-lambda token))


(define (make-label-for-lambda)
  (make-label-for-lambda-with-token (make-label-token)))


(define (make-label-for-closure-with-token token)
  (make-label 'scheme-closure token))


