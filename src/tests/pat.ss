(need pat/pat)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Simple pattern cases
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern `(export (,symbol? export-symbol)))
(define replace `((replace-stuff (,symbol? export-symbol))))

(define match-1 (pattern-match pattern '(export 3)))
(write match-1)
(newline)
(newline)

(define match-2 (pattern-match pattern '(export foo)))
(write match-2)
(newline)
(write (pattern-match-replace replace match-2))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; A simple pattern using an improper list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-2 `(global (,symbol? global-symbol) . (? rest)))
(define replace-2 `(global-stuff the-symbol-is: (? global-symbol)
                                 the-rest-is: (? rest)))

(define match-3 (pattern-match pattern-2 '(global foo (const 1))))
(write match-3)
(newline)
(write (pattern-match-replace replace-2 match-3))
(newline)
(newline)

(define match-4 (pattern-match pattern-2 '(global foo (const 1) (label x))))
(write match-4)
(newline)
(write (pattern-match-replace replace-2 match-4))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the list-match operator (@) using an improper list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-3 `(global (,symbol? global-symbol)
                           .
                           (@ ,(lambda (x) (> x 3)) symbol-list)))
(define replace-3 `(global-stuff the-symbol-is: (? global-symbol)
                                 the-rest-is: (? symbol-list)))

(define match-5 (pattern-match pattern-3 '(global foo 1 2 3)))
(write match-5)
(newline)
(newline)

(define match-6 (pattern-match pattern-3 '(global foo 4 5 6)))
(write match-6)
(newline)
(write (pattern-match-replace replace-3 match-6))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the list-match operator (@) with a proper list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-4 `(global (,symbol? global-symbol)
                           (@ ,(lambda (x) (> x 3)) symbol-list)))
(define replace-4 `(global-stuff the-symbol-is: (? global-symbol)
                                 the-rest-is: (? symbol-list)))

(define match-7 (pattern-match pattern-4 '(global foo (1 2 3))))
(write match-7)
(newline)
(newline)

(define match-8 (pattern-match pattern-4 '(global foo (4 5 6))))
(write match-8)
(newline)
(write (pattern-match-replace replace-4 match-8))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the list-match operator with transform and an improper list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-5 `(global (,symbol? global-symbol)
                           .
                           (@ ,(lambda (x) (and (number? x) (> x 3)))
                              ,(lambda (x) (+ x 1))
                              symbol-list)))
(define replace-5 `(global-stuff the-symbol-is: (? global-symbol)
                                 the-rest-is: (? symbol-list)))

(define match-9 (pattern-match pattern-5 '(global foo 1 2 3)))
(write match-9)
(newline)
(newline)

(define match-10 (pattern-match pattern-5 '(global foo 4 5 6)))
(write match-10)
(newline)
(write (pattern-match-replace replace-5 match-10))
(newline)
(newline)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test the list-match operator with predicate-transform and an
;; improper list
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define pattern-6 `(global (,symbol? global-symbol)
                           .
                           (@@ ,(lambda (x) (and (number? x)
                                                 (> x 3)
                                                 (+ x 5)))
                               symbol-list)))
(define replace-6 `(global-stuff the-symbol-is: (? global-symbol)
                                 the-rest-is: (? symbol-list)))

(define match-11 (pattern-match pattern-6 '(global foo 1 2 3)))
(write match-11)
(newline)
(newline)

(define match-12 (pattern-match pattern-6 '(global foo 4 5 6)))
(write match-12)
(newline)
(write (pattern-match-replace replace-6 match-12))
(newline)
(newline)
