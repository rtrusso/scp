(define str "#xdeadface")
(define n (string-length str))
(define ss (substring str 2 n))
(display ss)
(newline)
(define x (string->number ss 16))
(display x)
(newline)

(display "-2147483639->")
(display (string->number "-2147483639"))
(newline)

(display "2147483647->")
(display (string->number "2147483647"))
(newline)

(display "2147483648->")
(display (string->number "2147483648"))
(newline)

(display "-2147483647->")
(display (string->number "-2147483647"))
(newline)

(display "-2147483648->")
(display (string->number "-2147483648"))
(newline)

(display "#xffffffff->")
(display (string->number "ffffffff" 16))
(newline)

(display "#xcafebabe->")
(display (string->number "cafebabe" 16))
(newline)

(display "-889275714->")
(display (string->number "-889275714"))
(newline)
