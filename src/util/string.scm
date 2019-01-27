;; util/string.scm
;; String processing utility functions

(define (split-string string delimiters)
  (define (iter res tok str)
    (cond ((null? str) (if (not (null? tok))
                           (reverse (cons (list->string (reverse tok)) res))
                           (reverse res)))
          ((member (car str) delimiters) (if (not (null? tok))
                                             (iter (cons (list->string (reverse tok))
                                                         res)
                                                   '()
                                                   (cdr str))
                                             (iter res '() (cdr str))))
          (else (iter res (cons (car str) tok) (cdr str)))))
  (iter '() '() (string->list string)))

(define (tokenize-string string delimiters)
  (define (iter res tok str)
    (cond ((null? str) 
           (reverse (cons (list->string (reverse tok)) res)))
          ((member (car str) delimiters)
           (iter (cons (list->string (reverse tok))
                       res)
                 '()
                 (cdr str)))
          (else (iter res
                      (cons (car str) tok)
                      (cdr str)))))
  (iter '() '() (string->list string)))

(define (strlen string)
  (string-length string))

(define (downcase s)
  (cond
   ((char? s) (char-downcase s))
   ((string? s) (list->string (map char-downcase (string->list s))))))

(define (upcase s)
  (cond
   ((char? s) (char-upcase s))
   ((string? s) (list->string (map char-upcase (string->list s))))))

(define (ends-with? string suffix)
  (cond 
   ((> (strlen suffix) (strlen string)) #f)
   (else (string=? (substring string 
                              (- (strlen string) (strlen suffix))
                              (strlen string))
                   suffix))))

(define (starts-with? string prefix)
  (cond
   ((> (strlen prefix) (strlen string)) #f)
   (else (string=? (substring string
                              0
                              (strlen prefix))
                   prefix))))

(define (unquote-string string)
  (if (and (starts-with? string "\"")
           (ends-with? string "\""))
      (substring string 1 (- (string-length string) 1))
      string))

(define (string-last-char string)
  (string-ref string (- (string-length string) 1)))

(define (string-ends-with? s suffix)
  (and (< (string-length suffix) (string-length s))
       (string=? (substring s 
                            (- (string-length s) (string-length suffix))
                            (string-length s))
                 suffix)))

(define (string-ci-ends-with? s suffix)
  (and (< (string-length suffix) (string-length s))
       (string-ci=? (substring s 
                            (- (string-length s) (string-length suffix))
                            (string-length s))
                    suffix)))

(define (string-starts-with? s suffix)
  (and (< (string-length suffix) (string-length s))
       (string=? (substring s 0 (string-length suffix))
                 suffix)))

(define (string-ci-starts-with? s suffix)
  (and (< (string-length suffix) (string-length s))
       (string-ci=? (substring s 0 (string-length suffix))
                    suffix)))

(define (string-last-index-of string char)
  (let ((rest (member char (reverse (string->list string)))))
    (if rest
        (- (string-length string) (length rest))
        -1)))

(define (string-index-of string char)
  (let ((rest (member char (string->list string))))
    (if rest
        (- (string-length string) (length rest))
        -1)))

(define (string-contains-char? string char)
  (> (string-index-of string char) -1))

(define (escape-string proc s)
  (apply string-append
         (map (lambda (s) (apply string s))
              (map proc (string->list s)))))

(define (append-strings s)
  (apply string-append s))

(define (space-out-strings s . sep)
  (define (iter r l)
    (if (null? l)
        r
        (iter (string-append r (car sep) (car l))
              (cdr l))))
  (cond ((null? sep) (space-out-strings s " "))
        ((null? s) "")
        (else (iter (car s) (cdr s)))))

(define separate-strings space-out-strings)

(define (lines . strings)
  (space-out-strings strings (string #\newline)))

(define (indent n . strings)
  (apply lines
         (let ((space (make-string n #\space)))
         (map (lambda (line) (string-append space line))
              strings))))

(define (replace-characters-in-string string a b)
  (list->string (map (lambda (char)
                       (cond ((char=? a char) b)
                             (else char)))
                     (string->list string))))

(define (build-strchr char=?)
  (lambda (str chr)
    (let loop ((i 0))
      (cond ((>= i (string-length str)) #f)
            ((char=? chr (string-ref str i)) i)
            (else (loop (+ i 1)))))))

(define (build-strrchr char=?)
  (lambda (str chr)
    (let loop ((i (- (string-length str) 1)))
      (cond ((<= i 0) #f)
            ((char=? chr (string-ref str i)) i)
            (else (loop (- i 1)))))))

(define (build-strstr string=?)
  (lambda (str1 key)
    (let ((len (string-length str1))
          (key-len (string-length key)))
      (let loop ((i 0))
        (cond ((>= i (string-length str1)) 
               #f)
              ((string=? (substring str1 i (min len (+ i key-len)))
                         key)
               i)
              (else (loop (+ i 1))))))))

(define (string-reverse s)
  (list->string (reverse (string->list s))))

(define strstr (build-strstr string=?))
(define strchr (build-strchr char=?))
(define strrchr (build-strrchr char=?))
(define strrchr* (build-strrchr strchr))
(define strstr-ci (build-strstr string-ci=?))
(define strchr-ci (build-strchr char-ci=?))
(define strrchr-ci (build-strchr char-ci=?))

(define (build-match-wildcards char=?)
  (define (first string)
    (string-ref string 0))
  (define (rest string)
    (substring string 1 (string-length string)))
  (define (combine a b)
    (and a b (append (list a) b)))
  (lambda (pattern string)
    (let loop ((pattern pattern)
               (string string))
      (cond ((zero? (string-length pattern))
             (and (zero? (string-length string))
                  '()))
            ((char=? #\? (first pattern))
             (and (= (string-length pattern) (string-length string))
                  (combine (first string)
                           (loop (rest pattern) (rest string)))))
            ((char=? #\* (first pattern))
             (let inner ((i (string-length string)))
               (if (negative? i)
                   #f
                   (or (combine (substring string 0 i)
                                (loop (rest pattern) 
                                      (substring string i 
                                                 (string-length string))))
                       (inner (- i 1))))))
            (else
             (and (> (string-length string) 0)
                  (char=? (first pattern) (first string))
                  (loop (rest pattern) (rest string))))))))

(define match-wildcards (build-match-wildcards char=?))
(define match-wildcards-ci (build-match-wildcards char-ci=?))


(define (string-first string)
  (and (positive? (string-length string))
       (string-ref string 0)))

(define (string-rest string)
  (and (positive? (string-length string))
       (substring string 1 (string-length string))))

(define (string-prefix string i)
  (substring string 0 i))

(define (string-postfix string i)
  (substring string i (string-length string)))

(define (string-suffix string i)
  (substring string
             (- (string-length string) i)
             (string-length string)))

(define (string-remove-suffix string suffix)
  (if (string-ends-with? string suffix)
      (let* ((index (- (string-length string) (string-length suffix)))
             (no-suffix (string-prefix string index)))
        no-suffix)
      string))

; alternative split-string implementation
;(define (split-string string delim)
;  (define (iter res i)
;    (cond ((>= i (string-length string))
;           (reverse res))
;          ((strchr delim (string-ref string i))
;           (iter res (+ i 1)))
;          (else
;           (let ((next (let loop ((j (+ i 1)))
;                         (if (or (>= j (string-length string))
;                                 (strchr delim (string-ref string j)))
;                             j
;                             (loop (+ j 1))))))
;             (iter (cons (substring string i next) res) next)))))
;  (iter '() 0))


(define (string-buffer)
  (cons (make-string 64) 0))

(define (string-buffer-ensure-capacity! buf cap)
  (define (str-copy! src dst)
    (let loop ((i 0))
      (if (< i (string-length src))
          (begin (string-set! dst i (string-ref src i))
                 (loop (+ i 1))))))
  (if (< (string-length (car buf)) cap)
      (let ((new-buf (make-string (+ cap (quotient cap 2)))))
        (str-copy! (car buf) new-buf)
        (set-car! buf new-buf))))

(define (string-buffer-append! buf chr)
  (string-buffer-ensure-capacity! buf (+ (cdr buf) 1))
  (string-set! (car buf) (cdr buf) chr)
  (set-cdr! buf (+ (cdr buf) 1)))

(define (string-buffer->string buf)
  (let ((str (make-string (cdr buf))))
    (let loop ((i 0))
      (if (< i (string-length str))
          (begin (string-set! str i (string-ref (car buf) i))
                 (loop (+ i 1)))
          str))))

(define (string-strip-prefix string prefix)
  (if (string-starts-with? string prefix)
      (substring string
                 (string-length prefix)
                 (string-length string))
      string))

;(define string-buffer native-string-buffer)
;(define string-buffer-append! native-string-buffer-append!)
;(define string-buffer->string native-string-buffer->string)
