(need regex/regex)

(define (test x y)
  (if (equal? x y)
      (display "test passed\n")
      (display "test failed\n")))

(define (good-batch rgx . strings)
  (for-each (lambda (str) (test (regex-match-complete? rgx str) #t))
            strings))

(define (bad-batch rgx . strings)
  (for-each (lambda (str) (test (regex-match-complete? rgx str) #f))
            strings))

(good-batch '(* a) "" "a" "aa" "aaa" "aaaa" "aaaaa")
(bad-batch '(* a) "b" "ba" "aaaaaaaaas" "saa" "abcd")

;; (good-batch '(: a b c) "a" "b" "c" "a" "b" "c" "c" "b" "a" "b" "c" "a")
;; (bad-batch '(: a b c) "" "ba" "bb" "bx" "s" "d" "x" "jkl;asdf")

;; (good-batch '(+ a) "a" "aaa" "aa" "aaaaaaaaaaaaaaa")
;; (bad-batch '(+ a) "" "ab" "ba" "aaaaaaab" "baaaaaaaaaaaa")

;; (good-batch '(* (: a b c)) "" "a" "b" "c" "ab" "ca" "ccccca" "abcabcabcabc" "cbacbacba"
;;             "bacbac" "baccbaabc" "bbbbbbbbbbbbbbb")
;; (bad-batch '(* (: a b c)) "x" "abx" "axb" "ddd" "d" "q" "qqqq" "     3"
;;            "8387383###")
