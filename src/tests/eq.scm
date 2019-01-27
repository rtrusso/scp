(display (eq? '() '()))                 ;; true
(newline)
(display (eq? '() '(a)))                ;; false
(newline)
(display (eq? 1 1))                     ;; true - undefined in spec
(newline)
(display (eq? 1 0))                     ;; false
(newline)
(display (eq? #\f #\g))                 ;; false
(newline)
(display (eq? #\f #\f))                 ;; true - undefined in spec
(newline)
(display (eq? 'asdf 'asdf))             ;; true
(newline)
(display (eq? 'a 'b))                   ;; false
(newline)
(display (eq? 'a 'ab))                  ;; false
(newline)
