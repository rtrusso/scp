;;; r5rs-library.scm
;;; An implementation of the r5rs library procedures in Scheme.

;;; TODO: rationalize

;; equal?
(define (equal? a b)
  (define (vector-equal? a b)
    (define (iter i)
      (if (>= i (vector-length a))
          #t
          (and (equal? (vector-ref a i) (vector-ref b i)) (iter (+ i 1)))))
    (and (= (vector-length a) (vector-length b))
         (iter 0)))
  (define (pair-equal? a b)
    (if (or (not (pair? a)) (not (pair? b)))
        (equal? a b)
        (and (equal? (car a) (car b)) (equal? (cdr a) (cdr b)))))
  (define (string-equal? a b)
    (define (iter i)
      (if (>= i (string-length a))
           #t
           (and (char=? (string-ref a i) (string-ref b i))
                (iter (+ i 1)))))
     (and (= (string-length a) (string-length b))
          (iter 0)))
   (cond ((and (vector? a) (vector? b)) (vector-equal? a b))
         ((and (pair? a) (pair? b)) (pair-equal? a b))
         ((and (string? a) (string? b)) (string-equal? a b))
         (else (eqv? a b))))

(define (zero? z) (= z 0))
(define (positive? x) (> x 0))
(define (negative? x) (< x 0))
; TODO: revive
;(define (odd? n) (not (even? n)))
;(define (even? n) (zero? (remainder n 2)))

; r5rs specifies max requires at least 2 arguments but as an extension many
; scheme implementations tolerate calling it with a single argument, which
; results in code that depends on that behavior. For compatibility, it is also
; supported by this implementation.
(define (max num1 . numbers)
  (define (max2 a b) (if (> a b) a b))
  (define (iter r l)
    (if (null? l) r (iter (max2 r (car l)) (cdr l))))
  (if (null? numbers)
      num1
      (iter (max2 num1 (car numbers)) (cdr numbers))))

(define (min num1 . numbers)
  (define (min2 a b) (if (< a b) a b))
  (define (iter r l)
    (if (null? l) r (iter (min2 r (car l)) (cdr l))))
  (if (null? numbers)
      num1
      (iter (min2 num1 (car numbers)) (cdr numbers))))

(define (abs x)
  (if (negative? x) (- x) x))

; TODO: revive
;(define (gcd a b)
;  (if (zero? b)
;      a
;      (gcd b (remainder a b))))

; TODO: revive
;(define (lcm a b)
;  (/ (* a b) (gcd a b)))

;; TODO: fix this, revive
;(define (rationalize x y)
;  (/ (inexact->exact (numerator x))
;     (inexact->exact (denominator y))))

;(define (my-string->number s . def-radix-list)
;  (define def-radix (if (null? def-radixlist) 10 (car def-radix-list)))
;  (define valid-radices (list 2 8 10 16))
;  (define hex-digits '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3)
;                       (#\4 . 4) (#\5 . 5) (#\6 . 6) (#\7 . 7)
;                       (#\8 . 8) (#\9 . 9) (#\a . 10) (#\A . 10)
;                       (#\b . 11) (#\B . 11) (#\c . 12) (#\C . 12)
;                       (#\d . 13) (#\D . 13) (#\e . 14) (#\E . 14)
;                       (#\f . 15) (#\F . 15)))
;  (define decimal-digits '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)
;                           (#\5 . 5) (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9)))
;  (define binary-digits '((#\0 . 0) (#\1 . 1)))
;  (define octal-digits '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)
;                         (#\5 . 5) (#\6 . 6) (#\7 . 7)))
;  (define (parse-number digits base charmap)
;    (define (iter res digits)
;      (if (null? digits)
;          res
;          (iter (+ (* res base) (cdr (assoc (car digits) charmap)))
;                (cdr digits))))
;    (iter 0 digits))
;  (parse-number (string->list s) 16 hex-digits))

(define (not obj)
  (eq? #f obj))

(define (boolean? obj)
  (or (eq? #f obj)
      (eq? #t obj)))

(define (caar pair) (car (car pair)))
(define (cadr pair) (car (cdr pair)))
(define (cdar pair) (cdr (car pair)))
(define (cddr pair) (cdr (cdr pair)))
(define (caaar pair) (car (car (car pair))))
(define (caadr pair) (car (car (cdr pair))))
(define (cadar pair) (car (cdr (car pair))))
(define (caddr pair) (car (cdr (cdr pair))))
(define (cdaar pair) (cdr (car (car pair))))
(define (cdadr pair) (cdr (car (cdr pair))))
(define (cddar pair) (cdr (cdr (car pair))))
(define (cdddr pair) (cdr (cdr (cdr pair))))
(define (caaaar pair) (car (car (car (car pair)))))
(define (caaadr pair) (car (car (car (cdr pair)))))
(define (caadar pair) (car (car (cdr (car pair)))))
(define (caaddr pair) (car (car (cdr (cdr pair)))))
(define (cadaar pair) (car (cdr (car (car pair)))))
(define (cadadr pair) (car (cdr (car (cdr pair)))))
(define (caddar pair) (car (cdr (cdr (car pair)))))
(define (cadddr pair) (car (cdr (cdr (cdr pair)))))
(define (cdaaar pair) (cdr (car (car (car pair)))))
(define (cdaadr pair) (cdr (car (car (cdr pair)))))
(define (cdadar pair) (cdr (car (cdr (car pair)))))
(define (cdaddr pair) (cdr (car (cdr (cdr pair)))))
(define (cddaar pair) (cdr (cdr (car (car pair)))))
(define (cddadr pair) (cdr (cdr (car (cdr pair)))))
(define (cdddar pair) (cdr (cdr (cdr (car pair)))))
(define (cddddr pair) (cdr (cdr (cdr (cdr pair)))))

(define (null? obj)
  (eq? '() obj))

(define (list? pair)
  (define (iter slow fast)
    (cond ((or (null? slow) (null? fast)) #t)
          ((or (not (pair? slow)) (not (pair? fast))) #f)
          ((eq? slow fast) #f)
          ((null? (cdr fast)) #t)
          ((not (pair? (cdr fast))) #f)
          (else (iter (cdr slow) (cddr fast)))))
  (or (null? pair)
      (and (pair? pair)
           (iter pair (cdr pair)))))

(define (list . args) args)

(define (length list)
  (define (iter i l)
    (if (null? l) i (iter (+ i 1) (cdr l))))
  (iter 0 list))

(define (append list . lists)
  (define (append2 a b)
    (if (null? a)
        b
        (cons (car a) (append2 (cdr a) b))))
  (define (iter r l)
    (if (null? l)
        r
        (iter (append2 r (car l)) (cdr l))))
  (iter list lists))

(define (reverse list)
  (define (iter r l)
    (if (null? l)
        r
        (iter (cons (car l) r)
              (cdr l))))
  (iter '() list))

(define (list-tail list k)
  (if (zero? k)
      list
      (list-tail (cdr list) (- k 1))))

(define (list-ref list k)
  (if (zero? k)
      (car list)
      (list-ref (cdr list) (- k 1))))

(define (memq obj list)
  (cond ((null? list) #f)
        ((eq? obj (car list)) list)
        (else (memq obj (cdr list)))))

(define (memv obj list)
  (cond ((null? list) #f)
        ((eqv? obj (car list)) list)
        (else (memv obj (cdr list)))))

(define (member obj list)
  (cond ((null? list) #f)
        ((equal? obj (car list)) list)
        (else (member obj (cdr list)))))

(define (assq obj alist)
  (cond ((null? alist) #f)
        ((eq? obj (caar alist)) (car alist))
        (else (assq obj (cdr alist)))))

(define (assv obj alist)
  (cond ((null? alist) #f)
        ((eqv? obj (caar alist)) (car alist))
        (else (assv obj (cdr alist)))))

(define (assoc obj alist)
  (cond ((null? alist) #f)
        ((equal? obj (caar alist)) (car alist))
        (else (assoc obj (cdr alist)))))

(define (char-ci=? char1 char2)
  (char=? (char-downcase char1) (char-downcase char2)))

(define (char-ci<? char1 char2)
  (char<? (char-downcase char1) (char-downcase char2)))

(define (char-ci>? char1 char2)
  (char>? (char-downcase char1) (char-downcase char2)))

(define (char-ci<=? char1 char2)
  (char<=? (char-downcase char1) (char-downcase char2)))

(define (char-ci>=? char1 char2)
  (char>=? (char-downcase char1) (char-downcase char2)))

(define (char-alphabetic? char)
  (or (char-upper-case? char) (char-lower-case? char)))

(define (char-numeric? char)
  (if (member char
              (list #\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9))
      #t
      #f))

(define (char-whitespace? char)
  (if (member char
              (list #\space #\newline (integer->char 13) (integer->char 9)))
      #t
      #f))

(define (char-lower-case? char)
  (if (member char
              (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
      #t
      #f))

(define (char-upper-case? char)
  (if (member char
              (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
      #t
      #f))

(define (char-upcase char)
  (let ((a (assoc char '((#\a . #\A) (#\b . #\B) (#\c . #\C) (#\d . #\D)
                         (#\e . #\E) (#\f . #\F) (#\g . #\G) (#\h . #\H)
                         (#\i . #\I) (#\j . #\J) (#\k . #\K) (#\l . #\L)
                         (#\m . #\M) (#\n . #\N) (#\o . #\O) (#\p . #\P)
                         (#\q . #\Q) (#\r . #\R) (#\s . #\S) (#\t . #\T)
                         (#\u . #\U) (#\v . #\V) (#\w . #\W) (#\x . #\X)
                         (#\y . #\y) (#\z . #\Z)))))
    (if a (cdr a) char)))

(define (char-downcase char)
  (let ((a (assoc char '((#\A . #\a) (#\B . #\b) (#\C . #\c) (#\D . #\d)
                         (#\E . #\e) (#\F . #\f) (#\G . #\g) (#\H . #\h)
                         (#\I . #\i) (#\J . #\j) (#\K . #\k) (#\L . #\l)
                         (#\M . #\m) (#\N . #\n) (#\O . #\o) (#\P . #\p)
                         (#\Q . #\q) (#\R . #\r) (#\S . #\s) (#\T . #\t)
                         (#\U . #\u) (#\V . #\v) (#\W . #\w) (#\X . #\x)
                         (#\Y . #\y) (#\Z . #\z)))))
    (if a (cdr a) char)))

(define (string . chars)
  (list->string chars))

(define (string=? s1 s2)
  (define (iter i)
    (cond ((>= i (string-length s1)) #t)
          ((char=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else #f)))
  (and (= (string-length s1) (string-length s2))
       (iter 0)))

(define (string-ci=? s1 s2)
  (define (iter i)
    (cond ((>= i (string-length s1)) #t)
          ((char-ci=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else #f)))
  (if (= (string-length s1) (string-length s2))
      (iter 0)
      #f))

(define (string<? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (< (string-length s1) (string-length s2)))
          ((char=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char<? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (string>? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (> (string-length s1) (string-length s2)))
          ((char=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char>? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (string<=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (<= (string-length s1) (string-length s2)))
          ((char=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char<=? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (string>=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (>= (string-length s1) (string-length s2)))
          ((char=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char>=? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (string-ci<? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (< (string-length s1) (string-length s2)))
          ((char-ci=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char-ci<? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (string-ci>? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (> (string-length s1) (string-length s2)))
          ((char-ci=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char-ci>? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (string-ci<=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (<= (string-length s1) (string-length s2)))
          ((char-ci=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char-ci<=? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (string-ci>=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string-length s1))
               (>= i (string-length s2)))
           (>= (string-length s1) (string-length s2)))
          ((char-ci=? (string-ref s1 i) (string-ref s2 i)) (iter (+ i 1)))
          (else (char-ci>=? (string-ref s1 i) (string-ref s2 i)))))
  (iter 0))

(define (substring string start end)
  (define (iter r i)
    (if (< i end)
        (iter (cons (string-ref string i) r) (+ i 1))
        (list->string (reverse r))))
  (iter '() start))

(define (string-append s1 . s2)
  (define (append2 s1 s2)
    (list->string (append (string->list s1) (string->list s2))))
  (define (iter res args)
    (if (null? args)
        res
        (iter (append2 res (car args)) (cdr args))))
  (iter s1 s2))

(define (string->list string)
  (define (iter r i)
    (if (>= i 0)
        (iter (cons (string-ref string i) r) (- i 1))
        r))
  (iter '() (- (string-length string) 1)))

(define (list->string list)
  (let ((s (make-string (length list))))
    (define (iter i l)
      (if (null? l)
          s
          (begin (string-set! s i (car l)) (iter (+ i 1) (cdr l)))))
    (iter 0 list)))

(define (string-copy string)
  (list->string (string->list string)))

(define (string-fill! string char)
  (let ((len (string-length string)))
    (define (iter i)
      (if (< i len)
          (begin (string-set! string i char)
                 (iter (+ i 1)))))
    (iter 0)))

(define (vector . args)
  (list->vector args))

(define (vector->list vec)
  (define (iter r i)
    (if (>= i 0)
        (iter (cons (vector-ref vec i) r) (- i 1))
        r))
  (iter '() (- (vector-length vec) 1)))

(define (list->vector list)
  (let ((vec (make-vector (length list))))
    (define (iter i l)
      (if (not (null? l))
          (begin (vector-set! vec i (car l)) (iter (+ i 1) (cdr l)))
          vec))
    (iter 0 list)))

(define (vector-fill! vec obj)
  (define (iter i)
    (if (< i (vector-length vec))
        (begin (vector-set! vec i obj) (iter (+ i 1)))))
  (iter 0))

(define (map proc list . lists)
  (define (any-null? list)
    (cond ((null? list) #f)
          ((null? (car list)) #t)
          (else (any-null? (cdr list)))))
  (define (cars list)
    (define (iter r list)
      (if (null? list)
          (reverse r)
          (iter (cons (caar list) r) (cdr list))))
    (iter '() list))
  (define (cdrs list)
    (define (iter r list)
      (if (null? list)
          (reverse r)
          (iter (cons (cdar list) r) (cdr list))))
    (iter '() list))
  (define (iter res lists)
    (if (any-null? lists)
        (reverse res)
        (iter (cons (apply proc (cars lists)) res)
              (cdrs lists))))
  (if (null? lists)
      (let loop ((r '()) (l list))
        (if (null? l)
            (reverse r)
            (loop (cons (proc (car l)) r)
                  (cdr l))))
      (iter '() (cons list lists))))

(define (for-each proc list . lists)
  (define (any-null? list)
    (cond ((null? list) #f)
          ((null? (car list)) #t)
          (else (any-null? (cdr list)))))
  (define (iter lists)
    (if (and (not (null? lists)) (not (any-null? lists)))
        (begin (apply proc (map car lists))
               (iter (map cdr lists)))))
  (if (null? lists)
      (let loop ((l list))
        (if (not (null? l))
            (begin (proc (car l))
                   (loop (cdr l)))))
      (iter (cons list lists))))

(define (force promise)
  (promise))

(define (call-with-input-file string proc)
  (let ((file (open-input-file string)))
    (let ((val (proc file)))
      (close-input-port file)
      val)))

(define (call-with-output-file string proc)
  (let ((file (open-output-file string)))
    (let ((val (proc file)))
      (close-output-port file)
      val)))

;#DEFINE READER_EXTENSIONS
;#IFDEF READER_EXTENSIONS
(define *reader-extensions* '())

(define (install-reader-extension start reader)
  (set! *reader-extensions* (cons (cons start reader)
                                  *reader-extensions*)))
;#ENDIF READER_EXTENSIONS

(define (read . port)
  (define (read port)
    (define standard-delimiters
      (list->string (list #\space #\( #\) #\newline #\" #\; #\"
                          (integer->char 13) (integer->char 9))))
    (define (eat char)
      (if (or (and (char? char) (char=? char (read-char port)))
              (member (read-char port) char))
          #t
          (error "Expecting other character -- READ" port char)))
    (define (read-until char)
      (define (iter res)
        (if (or (eof-object? (peek-char port))
                (and (char? char) (char=? (peek-char port) char))
                (and (string? char) (member (peek-char port)
                                      (string->list char)))
                (and (list? char) (member (peek-char port) char)))
            (list->string (reverse res))
            (iter (cons (read-char port) res))))
      (iter '()))
    (define (eat-comment)
      (read-until #\newline))
    (define (eat-whitespace)
      (if (char-whitespace? (peek-char port))
          (begin (read-char port) (eat-whitespace))
          (if (char=? #\; (peek-char port))
              (begin (eat-comment)
                     (eat-whitespace)
                     ))))
    (define (fix-improper head tail)
      (if (null? head)
          tail
          (fix-improper (cdr head) (cons (car head) tail))))
    (define (read-list l)
      (eat-whitespace)
      (cond ((eof-object? (peek-char port))
             (error
              "Input was terminated while looking for a closing parens."))
            ((char=? (peek-char port) #\)) (read-char port) (reverse l))
            (else
             (let ((elem (read port)))
               (if (and (symbol? elem) (string=? "." (symbol->string elem)))
                   (let ((ans (fix-improper l (read port))))
                     (eat-whitespace)
                     (eat #\))
                     ans)
                   (read-list (cons elem l)))))))
    (define (read-string res)
      (let ((char (read-char port)))
        (cond ((eof-object? char) (list->string (reverse res)))
              ((char=? #\\ char)
               (let* ((next-char (read-char port))
                      (entry (cond ((char=? next-char #\t) (integer->char #x09))
                                   ((char=? next-char #\r) (integer->char #x0d))
                                   ((char=? next-char #\n) #\newline)
                                   ((char=? next-char #\\) #\\)
                                   ((char=? next-char #\") #\")
                                   (else #f))))
                 (if entry
                     (read-string (cons entry res))
                     (error "Invalid character escape - " next-char))))
              ((char=? #\" char) (list->string (reverse res)))
              (else (read-string (cons char res))))))
    (define (read-char-lit)
      (eat #\\)
      (case (peek-char port)
        ((#\; #\( #\) #\" #\;) (read-char port))
        ((#\space #\newline (integer->char 13))
         (error "Invalid character constant." (peek-char port)))
        (else
         (if (eof-object? (peek-char port))
             (error "Reached end-of-file in character constant.")
             (let ((name (read-until standard-delimiters)))
               (if (= (string-length name) 1)
                   (string-ref name 0)
                   (let ((char (cond ((string=? name "newline") #\newline)
                                     ((string=? name "carriage-return") (integer->char #x0d))
                                     ((string=? name "space") #\space)
                                     ((string=? name "tab") (integer->char #x09))
                                     (else #f))
                               ))
                     (if char
                         char
                         (error "An invalid character name was specified."
                                name)))))))))
    (define (parse-id-or-num string)
      (define n (string-length string))
      (define special-initial '(#\! #\$ #\% #\& #\* #\/ #\: #\< #\= #\> #\? #\^ #\_ #\~ #\@))
      (define special-subsequent '(#\+ #\- #\. #\@))
      (define peculiar-identifier '("+" "-" "." "..."))
      (define (ident-initial? c)
        (or (char-alphabetic? c)
            (memv c special-initial)))
      (define (ident-subsequent? c)
        (or (ident-initial? c)
            (char-numeric? c)
            (member c special-subsequent)))
      (define (valid-id? i)
        (if (>= i n)
            #t
            (and (ident-subsequent? (string-ref string i))
                 (valid-id? (+ i 1)))))
      (if (zero? n)
          (error "Read - unable to parse the empty string")
          (let ((first (string-ref string 0)))
            (cond ((member string peculiar-identifier)
                   (string->symbol string))
                  ((or (char-numeric? first)
                       (char=? first #\-))
                   (let ((result (string->number string)))
                     (if (not result)
                         (error "Unable to parse string as number" string)
                         result)))
                  ((ident-initial? first)
                   (if (valid-id? 1)
                       (string->symbol string)
                       (error "Unable to parse string as symbol" string)))
                  ((and (char=? #\# first)
                        (> n 1)
                        (char=? #\x (string-ref string 1)))
                   (let ((result (string->number (substring string 2 n) 16)))
                     (if result
                         result
                         (error "Unable to parse hex string as number" string))))
                  (else
                   (error "Unrecognized token" string))))))
    (let* ((char (read-char port))
           (next (if (eof-object? char) char (peek-char port))))
      (cond ((eof-object? char) char)
            ((char=? #\; char) (begin (eat-comment)
                                      (read port)))
            ((char-whitespace? char) (read port))
            ((and (char=? #\# char) (char=? #\\ next))
             (read-char-lit))
            ((and (char=? #\# char) (char=? #\t (char-downcase next)))
             (begin (read-char port)
                    #t))
            ((and (char=? #\# char) (char=? #\f (char-downcase next)))
             (begin (read-char port)
                    #f))
            ((and (char=? #\# char) (char=? #\( next))
             (read-char port) (list->vector (read-list '())))
;#IFDEF READER_EXTENSIONS
            ((and (char=? #\# char) (assoc next *reader-extensions*))
             (let ((reader (cdr (assoc next *reader-extensions*))))
               (reader port)))
;#ENDIF READER_EXTENSIONS
            ((char=? #\' char) (list 'quote (read port)))
            ((char=? #\` char) (list 'quasiquote (read port)))
            ((char=? #\, char) (if (char=? next #\@)
                                   (begin (read-char port)
                                          (list 'unquote-splicing
                                                (read port)))
                                   (list 'unquote (read port))))
            ((char=? #\" char) (read-string '()))
            ((char=? #\( char)

             (read-list '()))
            (else
             (let ((the-string (string-append
                                (string char)
                                (read-until standard-delimiters))))
               (parse-id-or-num the-string))))))
  (read (if (null? port)
            (current-input-port)
            (car port))))

(define (write obj . port-list)
  (if (null? port-list)
      (write obj (current-output-port))
      (let ((port (car port-list))
            (wri (lambda objs (for-each (lambda (obj)
                                          (write obj (car port-list)))
                                        objs)))
            (disp (lambda objs (for-each (lambda (obj)
                                           (display obj (car port-list)))
                                         objs))))
        (define (char-name char)
          (case char
            ((#\newline) "newline")
            ((#\space) "space")
            (else (cond ((char=? char (integer->char #x09)) "tab")
                        ((char=? char (integer->char #x0d)) "carriage-return")
                        (else (string char))))))
        (define (wri-char char)
          (cond ((char=? char (integer->char #x09))
                 (disp #\\) (disp #\t))
                ((char=? char (integer->char #x0d))
                 (disp #\\) (disp #\r))
                ((char=? char #\newline)
                 (disp #\\) (disp #\n))
                (else
                 (case char
                   ((#\") (disp #\\) (disp #\"))
                   ((#\\) (disp #\\) (disp #\\))
                   (else (disp char))))))
        (define (wri-pair prefix obj)
          (cond ((null? obj) (disp ")"))
                ((pair? obj)
                 (disp prefix) (wri (car obj)) (wri-pair " " (cdr obj)))
                (else (disp prefix "." prefix) (wri obj) (disp ")"))))
        (cond ((char? obj) (disp "#\\") (disp (char-name obj)))
              ((string? obj)
               (disp #\")
               (for-each wri-char (string->list obj))
               (disp #\"))
              ((pair? obj) (wri-pair "(" obj))
              ((vector? obj) (disp "#") (write (vector->list obj)))
              (else (disp obj))))))

(define (display obj . port-list)
  (if (null? port-list)
      (display obj (current-output-port))
      (let* ((port (car port-list))
             (disp (lambda (str)
                     (let loop ((index 0))
                       (if (< index (string-length str))
                           (begin (write-char (string-ref str index) port)
                                  (loop (+ index 1)))
                           str)))))

        (define (disp-vector vec i)
          (if (= i 0) (disp "#("))
          (if (>= i (vector-length vec))
              (disp ")")
              (begin (if (not (= 0 i)) (disp " "))
                     (display (vector-ref vec i) port)
                     (disp-vector vec (+ i 1)))))
        (define (disp-list prefix l)
          (cond ((pair? (cdr l))
                 (disp prefix)
                 (display (car l) port)
                 (disp-list " " (cdr l)))
                ((null? (cdr l))
                 (disp prefix)
                 (display (car l) port)
                 (disp ")"))
                (else
                 (disp prefix)
                 (display (car l) port)
                 (disp " . ")
                 (display (cdr l) port)
                 (disp ")"))))
        (cond ((string? obj) (disp obj))
              ((symbol? obj) (disp (symbol->string obj)))
              ((number? obj) (disp (number->string obj)))
              ((boolean? obj) (disp (if obj "#t" "#f")))
              ((char? obj) (write-char obj port))
              ((vector? obj) (disp-vector obj 0))
              ((and (list? obj) (= 2 (length obj)) (eqv? 'quote (car obj)))
               (begin (disp "'") (display (cadr obj) port)))
              ((pair? obj) (disp-list "(" obj))
              ((null? obj) (disp "()"))
              ((procedure? obj) (disp "#<procedure>"))
              ((and (input-port? obj) (output-port? obj))
               (disp "#<input-output-port>"))
              ((input-port? obj) (disp "#<input-port>"))
              ((output-port? obj) (disp "#<output-port>"))
              ((eof-object? obj) (disp "#<EOF>"))
              ; input port, output port, procedure, continuation, etc.
              (else (disp "#<undisplayable>"))))))

(define (newline . port)
  (if (null? port)
      (newline (current-output-port))
      (display #\newline (car port))))

(define (quotient numerator denominator)
  (define (sign x)
    (cond ((zero? x)
           0)
          ((negative? x)
           -1)
          (else
           1)))

  (define (iter-fast result remainder denominator depth)
    (let ((next (* denominator depth)))
      (if (< next remainder)
          (iter-fast (+ result depth)
                     (- remainder next)
                     denominator
                     next)
          (iter-slow result remainder denominator))))

  (define (iter-slow result remainder denominator)
    (if (< remainder denominator)
        result
        (iter-fast (+ result 1) (- remainder denominator) denominator 1)))

  (let ((result (iter-fast 0 (abs numerator) (abs denominator) 1)))
    (if (= (sign numerator) (sign denominator))
        result
        (- 0 result))))

(define (remainder numerator denominator)
  (let ((q (quotient numerator denominator)))
    (- numerator (* q denominator))))

(define (modulo numerator denominator)
  (define (sign x)
    (cond ((zero? x)
           0)
          ((negative? x)
           -1)
          (else
           1)))

  (let ((r (remainder numerator denominator)))
    (if (= (sign denominator) (sign r))
        r
        (+ denominator r))))

(define (number->string z . args)
  (define chars "0123456789abcdef")
  ;; Note that R5RS defines number->string as a procedure and not a
  ;; library procedure.  However right now we provide only a simple
  ;; integer implementation as a library procedure.
  (define (iter x result radix)
    (if (zero? x)
        (if (null? result)
            (list->string (list #\0))
            (list->string result))
        (iter (quotient x radix)
              (cons (string-ref chars (remainder x radix))
                    result)
              radix)))

  (define (inner x radix)
    (if (and (= x -2147483648)
             (= radix 10))
        "-2147483648"
        (let ((s (iter (abs x) '() radix)))
          (if (negative? x)
              (string-append "-" s)
              s))))

  (cond ((null? args)
         (inner z 10))
        ((or (not (null? (cdr args)))
             (not (integer? (car args)))
             (case (car args)
               ((2 8 10 16) #f)
               (else #t)))
         (error "Invalid radix specified" args))
        (else
         (inner z (car args)))))

(define (/ z . args)
  (define (iter result args)
    (if (null? args)
        result
        (iter (quotient result (car args))
              (cdr args))))

  (if (null? args)
      (quotient 1 z)
      (iter z args)))