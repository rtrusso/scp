/// r5rs_library.js
/// An implementation of the r5rs library procedures in JavaScript.

/// TODO: rationalize

// equalP
(define (equalP a b)
  (define (vector_equalP a b)
    (define (iter i)
      (if (>= i (vector_length a))
          true
          (and (equalP (vector_ref a i) (vector_ref b i)) (iter (+ i 1)))))
    (and (= (vector_length a) (vector_length b))
         (iter 0)))
  (define (pair_equalP a b)
    (if (or (not (pairP a)) (not (pairP b)))
        (equalP a b)
        (and (equalP (car a) (car b)) (equalP (cdr a) (cdr b)))))
  (define (string_equalP a b)
    (define (iter i)
      (if (>= i (string_length a))
           true
           (and (char_eqP (string_ref a i) (string_ref b i))
                (iter (+ i 1)))))
     (and (= (string_length a) (string_length b))
          (iter 0)))
   (cond ((and (vectorP a) (vectorP b)) (vector_equalP a b))
         ((and (pairP a) (pairP b)) (pair_equalP a b))
         ((and (stringP a) (stringP b)) (string_equalP a b))
         (else (eqvP a b))))

(define (zeroP z) (= z 0))
(define (positiveP x) (> x 0))
(define (negativeP x) (< x 0))
// TODO: revive
//(define (odd? n) (not (even? n)))
//(define (even? n) (zeroP (remainder n 2)))

// r5rs specifies max requires at least 2 arguments but as an extension many
// scheme implementations tolerate calling it with a single argument, which
// results in code that depends on that behavior. For compatibility, it is also
// supported by this implementation.
(define (max num1 . numbers)
  (define (max2 a b) (if (> a b) a b))
  (define (iter r l)
    (if (nullP l) r (iter (max2 r (car l)) (cdr l))))
  (if (nullP numbers)
      num1
      (iter (max2 num1 (car numbers)) (cdr numbers))))

(define (min num1 . numbers)
  (define (min2 a b) (if (< a b) a b))
  (define (iter r l)
    (if (nullP l) r (iter (min2 r (car l)) (cdr l))))
  (if (nullP numbers)
      num1
      (iter (min2 num1 (car numbers)) (cdr numbers))))

(define (abs x)
  (if (negativeP x) (subtract_operator x) x))

// TODO: revive
//(define (gcd a b)
//  (if (zeroP b)
//      a
//      (gcd b (remainder a b))))

// TODO: revive
//(define (lcm a b)
//  (/ (* a b) (gcd a b)))

// TODO: fix this, revive
//(define (rationalize x y)
//  (/ (inexact->exact (numerator x))
//     (inexact->exact (denominator y))))

//(define (my-string->number s . def-radix-list)
//  (define def-radix (if (nullP def-radixlist) 10 (car def-radix-list)))
//  (define valid-radices (list 2 8 10 16))
//  (define hex-digits '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3)
//                       (#\4 . 4) (#\5 . 5) (#\6 . 6) (#\7 . 7)
//                       (#\8 . 8) (#\9 . 9) (#\a . 10) (#\A . 10)
//                       (#\b . 11) (#\B . 11) (#\c . 12) (#\C . 12)
//                       (#\d . 13) (#\D . 13) (#\e . 14) (#\E . 14)
//                       (#\f . 15) (#\F . 15)))
//  (define decimal-digits '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)
//                           (#\5 . 5) (#\6 . 6) (#\7 . 7) (#\8 . 8) (#\9 . 9)))
//  (define binary-digits '((#\0 . 0) (#\1 . 1)))
//  (define octal-digits '((#\0 . 0) (#\1 . 1) (#\2 . 2) (#\3 . 3) (#\4 . 4)
//                         (#\5 . 5) (#\6 . 6) (#\7 . 7)))
//  (define (parse-number digits base charmap)
//    (define (iter res digits)
//      (if (nullP digits)
//          res
//          (iter (+ (* res base) (cdr (assoc (car digits) charmap)))
//                (cdr digits))))
//    (iter 0 digits))
//  (parse-number (string->list s) 16 hex-digits))

(define (not obj)
  (eqP false obj))

(define (booleanP obj)
  (or (eqP false obj)
      (eqP true obj)))

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

(define (nullP obj)
  (eqP '() obj))

(define (list? pair)
  (define (iter slow fast)
    (cond ((or (nullP slow) (nullP fast)) true)
          ((or (not (pairP slow)) (not (pairP fast))) false)
          ((eqP slow fast) false)
          ((nullP (cdr fast)) true)
          ((not (pairP (cdr fast))) false)
          (else (iter (cdr slow) (cddr fast)))))
  (or (nullP pair)
      (and (pairP pair)
           (iter pair (cdr pair)))))

(define (list . args) args)

(define (length list)
  (define (iter i l)
    (if (nullP l) i (iter (+ i 1) (cdr l))))
  (iter 0 list))

(define (append list . lists)
  (define (append2 a b)
    (if (nullP a)
        b
        (cons (car a) (append2 (cdr a) b))))
  (define (iter r l)
    (if (nullP l)
        r
        (iter (append2 r (car l)) (cdr l))))
  (iter list lists))

(define (reverse list)
  (define (iter r l)
    (if (nullP l)
        r
        (iter (cons (car l) r)
              (cdr l))))
  (iter '() list))

(define (list-tail list k)
  (if (zeroP k)
      list
      (list-tail (cdr list) (subtract_operator k 1))))

(define (list-ref list k)
  (if (zeroP k)
      (car list)
      (list-ref (cdr list) (subtract_operator k 1))))

(define (memq obj list)
  (cond ((nullP list) false)
        ((eqP obj (car list)) list)
        (else (memq obj (cdr list)))))

(define (memv obj list)
  (cond ((nullP list) false)
        ((eqvP obj (car list)) list)
        (else (memv obj (cdr list)))))

(define (member obj list)
  (cond ((nullP list) false)
        ((equalP obj (car list)) list)
        (else (member obj (cdr list)))))

(define (assq obj alist)
  (cond ((nullP alist) false)
        ((eqP obj (caar alist)) (car alist))
        (else (assq obj (cdr alist)))))

(define (assv obj alist)
  (cond ((nullP alist) false)
        ((eqvP obj (caar alist)) (car alist))
        (else (assv obj (cdr alist)))))

(define (assoc obj alist)
  (cond ((nullP alist) false)
        ((equalP obj (caar alist)) (car alist))
        (else (assoc obj (cdr alist)))))

(define (char-ci=? char1 char2)
  (char_eqP (char-downcase char1) (char-downcase char2)))

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
      true
      false))

(define (char-whitespace? char)
  (if (member char
              (list #\space #\newline (integer->char 13) (integer->char 9)))
      true
      false))

(define (char-lower-case? char)
  (if (member char
              (list #\a #\b #\c #\d #\e #\f #\g #\h #\i #\j #\k #\l #\m
                    #\n #\o #\p #\q #\r #\s #\t #\u #\v #\w #\x #\y #\z))
      true
      false))

(define (char-upper-case? char)
  (if (member char
              (list #\A #\B #\C #\D #\E #\F #\G #\H #\I #\J #\K #\L #\M
                    #\N #\O #\P #\Q #\R #\S #\T #\U #\V #\W #\X #\Y #\Z))
      true
      false))

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
    (cond ((>= i (string_length s1)) true)
          ((char_eqP (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else false)))
  (and (= (string_length s1) (string_length s2))
       (iter 0)))

(define (string-ci=? s1 s2)
  (define (iter i)
    (cond ((>= i (string_length s1)) true)
          ((char-ci=? (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else false)))
  (if (= (string_length s1) (string_length s2))
      (iter 0)
      false))

(define (string<? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (< (string_length s1) (string_length s2)))
          ((char_eqP (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char<? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (string>? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (> (string_length s1) (string_length s2)))
          ((char_eqP (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char>? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (string<=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (<= (string_length s1) (string_length s2)))
          ((char_eqP (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char<=? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (string>=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (>= (string_length s1) (string_length s2)))
          ((char_eqP (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char>=? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (string-ci<? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (< (string_length s1) (string_length s2)))
          ((char-ci=? (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char-ci<? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (string-ci>? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (> (string_length s1) (string_length s2)))
          ((char-ci=? (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char-ci>? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (string-ci<=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (<= (string_length s1) (string_length s2)))
          ((char-ci=? (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char-ci<=? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (string-ci>=? s1 s2)
  (define (iter i)
    (cond ((or (>= i (string_length s1))
               (>= i (string_length s2)))
           (>= (string_length s1) (string_length s2)))
          ((char-ci=? (string_ref s1 i) (string_ref s2 i)) (iter (+ i 1)))
          (else (char-ci>=? (string_ref s1 i) (string_ref s2 i)))))
  (iter 0))

(define (substring string start end)
  (define (iter r i)
    (if (< i end)
        (iter (cons (string_ref string i) r) (+ i 1))
        (list->string (reverse r))))
  (iter '() start))

(define (string-append s1 . s2)
  (define (append2 s1 s2)
    (list->string (append (string->list s1) (string->list s2))))
  (define (iter res args)
    (if (nullP args)
        res
        (iter (append2 res (car args)) (cdr args))))
  (iter s1 s2))

(define (string->list string)
  (define (iter r i)
    (if (>= i 0)
        (iter (cons (string_ref string i) r) (subtract_operator i 1))
        r))
  (iter '() (subtract_operator (string_length string) 1)))

(define (list->string list)
  (let ((s (make-string (length list))))
    (define (iter i l)
      (if (nullP l)
          s
          (begin (string-set! s i (car l)) (iter (+ i 1) (cdr l)))))
    (iter 0 list)))

(define (string-copy string)
  (list->string (string->list string)))

(define (string-fill! string char)
  (let ((len (string_length string)))
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
        (iter (cons (vector_ref vec i) r) (subtract_operator i 1))
        r))
  (iter '() (subtract_operator (vector_length vec) 1)))

(define (list->vector list)
  (let ((vec (make-vector (length list))))
    (define (iter i l)
      (if (not (nullP l))
          (begin (vector-set! vec i (car l)) (iter (+ i 1) (cdr l)))
          vec))
    (iter 0 list)))

(define (vector-fill! vec obj)
  (define (iter i)
    (if (< i (vector_length vec))
        (begin (vector-set! vec i obj) (iter (+ i 1)))))
  (iter 0))

(define (map proc list . lists)
  (define (any-nullP list)
    (cond ((nullP list) false)
          ((nullP (car list)) true)
          (else (any-nullP (cdr list)))))
  (define (cars list)
    (define (iter r list)
      (if (nullP list)
          (reverse r)
          (iter (cons (caar list) r) (cdr list))))
    (iter '() list))
  (define (cdrs list)
    (define (iter r list)
      (if (nullP list)
          (reverse r)
          (iter (cons (cdar list) r) (cdr list))))
    (iter '() list))
  (define (iter res lists)
    (if (any-nullP lists)
        (reverse res)
        (iter (cons (apply proc (cars lists)) res)
              (cdrs lists))))
  (if (nullP lists)
      (let loop ((r '()) (l list))
        (if (nullP l)
            (reverse r)
            (loop (cons (proc (car l)) r)
                  (cdr l))))
      (iter '() (cons list lists))))

(define (for-each proc list . lists)
  (define (any-nullP list)
    (cond ((nullP list) false)
          ((nullP (car list)) true)
          (else (any-nullP (cdr list)))))
  (define (iter lists)
    (if (and (not (nullP lists)) (not (any-nullP lists)))
        (begin (apply proc (map car lists))
               (iter (map cdr lists)))))
  (if (nullP lists)
      (let loop ((l list))
        (if (not (nullP l))
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
      (if (or (and (char? char) (char_eqP char (read-char port)))
              (member (read-char port) char))
          true
          (error "Expecting other character -- READ" port char)))
    (define (read-until char)
      (define (iter res)
        (if (or (eof-object? (peek-char port))
                (and (char? char) (char_eqP (peek-char port) char))
                (and (stringP char) (member (peek-char port)
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
          (if (char_eqP #\; (peek-char port))
              (begin (eat-comment)
                     (eat-whitespace)
                     ))))
    (define (fix-improper head tail)
      (if (nullP head)
          tail
          (fix-improper (cdr head) (cons (car head) tail))))
    (define (read-list l)
      (eat-whitespace)
      (cond ((eof-object? (peek-char port))
             (error
              "Input was terminated while looking for a closing parens."))
            ((char_eqP (peek-char port) #\)) (read-char port) (reverse l))
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
              ((char_eqP #\\ char)
               (let* ((next-char (read-char port))
                      (entry (cond ((char_eqP next-char #\t) (integer->char #x09))
                                   ((char_eqP next-char #\r) (integer->char #x0d))
                                   ((char_eqP next-char #\n) #\newline)
                                   ((char_eqP next-char #\\) #\\)
                                   ((char_eqP next-char #\") #\")
                                   (else false))))
                 (if entry
                     (read-string (cons entry res))
                     (error "Invalid character escape - " next-char))))
              ((char_eqP #\" char) (list->string (reverse res)))
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
               (if (= (string_length name) 1)
                   (string_ref name 0)
                   (let ((char (cond ((string=? name "newline") #\newline)
                                     ((string=? name "carriage-return") (integer->char #x0d))
                                     ((string=? name "space") #\space)
                                     ((string=? name "tab") (integer->char #x09))
                                     (else false))
                               ))
                     (if char
                         char
                         (error "An invalid character name was specified."
                                name)))))))))
    (define (parse-id-or-num string)
      (define n (string_length string))
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
            true
            (and (ident-subsequent? (string_ref string i))
                 (valid-id? (+ i 1)))))
      (if (zeroP n)
          (error "Read - unable to parse the empty string")
          (let ((first (string_ref string 0)))
            (cond ((member string peculiar-identifier)
                   (string->symbol string))
                  ((or (char-numeric? first)
                       (char_eqP first #\-))
                   (let ((result (string->number string)))
                     (if (not result)
                         (error "Unable to parse string as number" string)
                         result)))
                  ((ident-initial? first)
                   (if (valid-id? 1)
                       (string->symbol string)
                       (error "Unable to parse string as symbol" string)))
                  ((and (char_eqP #\# first)
                        (> n 1)
                        (char_eqP #\x (string_ref string 1)))
                   (let ((result (string->number (substring string 2 n) 16)))
                     (if result
                         result
                         (error "Unable to parse hex string as number" string))))
                  (else
                   (error "Unrecognized token" string))))))
    (let* ((char (read-char port))
           (next (if (eof-object? char) char (peek-char port))))
      (cond ((eof-object? char) char)
            ((char_eqP #\; char) (begin (eat-comment)
                                      (read port)))
            ((char-whitespace? char) (read port))
            ((and (char_eqP #\# char) (char_eqP #\\ next))
             (read-char-lit))
            ((and (char_eqP #\# char) (char_eqP #\t (char-downcase next)))
             (begin (read-char port)
                    true))
            ((and (char_eqP #\# char) (char_eqP #\f (char-downcase next)))
             (begin (read-char port)
                    false))
            ((and (char_eqP #\# char) (char_eqP #\( next))
             (read-char port) (list->vector (read-list '())))
;#IFDEF READER_EXTENSIONS
            ((and (char_eqP #\# char) (assoc next *reader-extensions*))
             (let ((reader (cdr (assoc next *reader-extensions*))))
               (reader port)))
;#ENDIF READER_EXTENSIONS
            ((char_eqP #\' char) (list 'quote (read port)))
            ((char_eqP #\` char) (list 'quasiquote (read port)))
            ((char_eqP #\, char) (if (char_eqP next #\@)
                                   (begin (read-char port)
                                          (list 'unquote-splicing
                                                (read port)))
                                   (list 'unquote (read port))))
            ((char_eqP #\" char) (read-string '()))
            ((char_eqP #\( char)

             (read-list '()))
            (else
             (let ((the-string (string-append
                                (string char)
                                (read-until standard-delimiters))))
               (parse-id-or-num the-string))))))
  (read (if (nullP port)
            (current-input-port)
            (car port))))

(define (write obj . port-list)
  (if (nullP port-list)
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
            (else (cond ((char_eqP char (integer->char #x09)) "tab")
                        ((char_eqP char (integer->char #x0d)) "carriage-return")
                        (else (string char))))))
        (define (wri-char char)
          (cond ((char_eqP char (integer->char #x09))
                 (disp #\\) (disp #\t))
                ((char_eqP char (integer->char #x0d))
                 (disp #\\) (disp #\r))
                ((char_eqP char #\newline)
                 (disp #\\) (disp #\n))
                (else
                 (case char
                   ((#\") (disp #\\) (disp #\"))
                   ((#\\) (disp #\\) (disp #\\))
                   (else (disp char))))))
        (define (wri-pair prefix obj)
          (cond ((nullP obj) (disp ")"))
                ((pairP obj)
                 (disp prefix) (wri (car obj)) (wri-pair " " (cdr obj)))
                (else (disp prefix "." prefix) (wri obj) (disp ")"))))
        (cond ((char? obj) (disp "#\\") (disp (char-name obj)))
              ((stringP obj)
               (disp #\")
               (for-each wri-char (string->list obj))
               (disp #\"))
              ((pairP obj) (wri-pair "(" obj))
              ((vectorP obj) (disp "#") (write (vector->list obj)))
              (else (disp obj))))))

(define (display obj . port-list)
  (if (nullP port-list)
      (display obj (current-output-port))
      (let* ((port (car port-list))
             (disp (lambda (str)
                     (let loop ((index 0))
                       (if (< index (string_length str))
                           (begin (write-char (string_ref str index) port)
                                  (loop (+ index 1)))
                           str)))))

        (define (disp-vector vec i)
          (if (= i 0) (disp "#("))
          (if (>= i (vector_length vec))
              (disp ")")
              (begin (if (not (= 0 i)) (disp " "))
                     (display (vector_ref vec i) port)
                     (disp-vector vec (+ i 1)))))
        (define (disp-list prefix l)
          (cond ((pairP (cdr l))
                 (disp prefix)
                 (display (car l) port)
                 (disp-list " " (cdr l)))
                ((nullP (cdr l))
                 (disp prefix)
                 (display (car l) port)
                 (disp ")"))
                (else
                 (disp prefix)
                 (display (car l) port)
                 (disp " . ")
                 (display (cdr l) port)
                 (disp ")"))))
        (cond ((stringP obj) (disp obj))
              ((symbol? obj) (disp (symbol->string obj)))
              ((number? obj) (disp (number->string obj)))
              ((booleanP obj) (disp (if obj "true" "false")))
              ((char? obj) (write-char obj port))
              ((vectorP obj) (disp-vector obj 0))
              ((and (list? obj) (= 2 (length obj)) (eqvP 'quote (car obj)))
               (begin (disp "'") (display (cadr obj) port)))
              ((pairP obj) (disp-list "(" obj))
              ((nullP obj) (disp "()"))
              ((procedure? obj) (disp "#<procedure>"))
              ((and (input-port? obj) (output-port? obj))
               (disp "#<input-output-port>"))
              ((input-port? obj) (disp "#<input-port>"))
              ((output-port? obj) (disp "#<output-port>"))
              ((eof-object? obj) (disp "#<EOF>"))
              ; input port, output port, procedure, continuation, etc.
              (else (disp "#<undisplayable>"))))))

(define (newline . port)
  (if (nullP port)
      (newline (current-output-port))
      (display #\newline (car port))))

(define (quotient numerator denominator)
  (define (sign x)
    (cond ((zeroP x)
           0)
          ((negativeP x)
           -1)
          (else
           1)))

  (define (iter-fast result remainder denominator depth)
    (let ((next (* denominator depth)))
      (if (< next remainder)
          (iter-fast (+ result depth)
                     (subtract_operator remainder next)
                     denominator
                     next)
          (iter-slow result remainder denominator))))

  (define (iter-slow result remainder denominator)
    (if (< remainder denominator)
        result
        (iter-fast (+ result 1) (subtract_operator remainder denominator) denominator 1)))

  (let ((result (iter-fast 0 (abs numerator) (abs denominator) 1)))
    (if (= (sign numerator) (sign denominator))
        result
        (subtract_operator 0 result))))

(define (remainder numerator denominator)
  (let ((q (quotient numerator denominator)))
    (subtract_operator numerator (* q denominator))))

(define (modulo numerator denominator)
  (define (sign x)
    (cond ((zeroP x)
           0)
          ((negativeP x)
           -1)
          (else
           1)))

  (let ((r (remainder numerator denominator)))
    (if (= (sign denominator) (sign r))
        r
        (+ denominator r))))

(define (number->string z . args)
  (define chars "0123456789abcdef")
  // Note that R5RS defines number->string as a procedure and not a
  // library procedure.  However right now we provide only a simple
  // integer implementation as a library procedure.
  (define (iter x result radix)
    (if (zeroP x)
        (if (nullP result)
            (list->string (list #\0))
            (list->string result))
        (iter (quotient x radix)
              (cons (string_ref chars (remainder x radix))
                    result)
              radix)))

  (define (inner x radix)
    (if (and (= x -2147483648)
             (= radix 10))
        "-2147483648"
        (let ((s (iter (abs x) '() radix)))
          (if (negativeP x)
              (string-append "-" s)
              s))))

  (cond ((nullP args)
         (inner z 10))
        ((or (not (nullP (cdr args)))
             (not (integer? (car args)))
             (case (car args)
               ((2 8 10 16) false)
               (else true)))
         (error "Invalid radix specified" args))
        (else
         (inner z (car args)))))

(define (/ z . args)
  (define (iter result args)
    (if (nullP args)
        result
        (iter (quotient result (car args))
              (cdr args))))

  (if (nullP args)
      (quotient 1 z)
      (iter z args)))