(define x (stat "tests/stat-dat.scm"))
(display (not (not x)))
(newline)
(display (vector? x)) ; stat buf is expected to be a vector
(newline)
(display (>= (vector-length x) 10)) ; minimum length of the vector
(newline)
(display (number? (vector-ref x 7))) ; file size
(newline)
(display (number? (vector-ref x 9))) ; mod time
(newline)
(display (= (vector-ref x 7) 7)) ; file size
(newline)
(define y (stat "thisfilereallydoesnotexist.scm"))
(display y) ; should be #f
(newline)
