(define obj (call-with-input-file "tests/read-dat4.scm"
              (lambda (f) (read f))))
(write obj)
(newline)

(if (not (list? obj))
    (error "bad input - not list"))

(if (null? obj)
    (error "bad input - null"))

(if (not (eq? 'define-symconst (car obj)))
    (error "bad input - incorrect header"))

(define entries (cdr obj))
(display "found ")
(display (length entries))
(display " entries")
(newline)

(if (not (list? obj))
    (error "bad cdr - no longer list"))

(for-each (lambda (x)
            (if (not (symbol? x))
                (error "bad name - should be symbol" x)))
          (map car entries))

(for-each (lambda (y)
            (if (not (integer? y))
                (begin (display "bad value - should be integer ")
                       (display y)
                       (newline)
                       (error "bad value - should be integer" y))))
          (map cadr entries))

(for-each (lambda (z)
            (display (car z))
            (display "=")
            (display (cadr z))
            (newline))
          entries)

;;     (define (parse-id-or-num string)
;;       (define n (string-length string))
;;       (define special-initial '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~))
;;       (define special-subsequent '(#\+ #\- #\. #\@))
;;       (define peculiar-identifier '("+" "-" "..."))
;;       (define (ident-initial? c)
;;         (or (char-alphabetic? c)
;;             (memv c special-initial)))
;;       (define (ident-subsequent? c)
;;         (or (ident-initial? c)
;;             (char-numeric? c)
;;             (member c special-subsequent)))
;;       (define (valid-id? i)
;;         (if (>= i n)
;;             #t
;;             (and (ident-subsequent? (string-ref string i))
;;                  (valid-id? (+ i 1)))))
;;       (if (zero? n)
;;           (error "Read - unable to parse the empty string")
;;           (let ((first (string-ref string 0)))
;;             (cond ((or (char-numeric? first)
;;                        (char=? first #\-))
;;                    (let ((result (string->number string)))
;;                      (if (not result)
;;                          (error "Unable to parse string as number" string)
;;                          result)))
;;                   ((ident-initial? first)
;;                    (if (valid-id? 1)
;;                        (string->symbol string)
;;                        (error "Unable to parse string as symbol" string)))
;;                   ((member string peculiar-identifier)
;;                    (string->symbol string))
;;                   ((and (char=? #\# first)
;;                         (> n 1)
;;                         (char=? #\x (string-ref string 1)))
;;                    (let ((result (string->number (substring string 2 n) 16)))
;;                      (if result
;;                          result
;;                          (error "Unable to parse hex string as number" string))))
;;                   (else
;;                    (error "Unrecognized token" string))))))

;; (define x (parse-id-or-num "-2147483639"))
;; (display "integer? ")
;; (display (integer? x))
;; (newline)
;; (display "symbol? ")
;; (display (symbol? x))
;; (newline)
;; (display "[")
;; (display x)
;; (display "]")
;; (newline)