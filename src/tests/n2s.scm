(display (number->string 3123)) ; 3123
(newline)
(display (number->string 0)) ; 0
(newline)
(display (number->string -1)) ; -1
(newline)
(display (number->string -3123)) ; -3123
(newline)
(display (number->string #x8000ffff 16)) ; 8000ffff (-2147418113)
(newline)
(display (number->string #xf000000f 16)) ; f000000f
(newline)
(display (number->string -2147483648)) ; -2147483648
(newline)
(display (number->string -2147483648 2)) ; -10000000000000000000000000000000
(newline)
(display (number->string -2147483648 8)) ; -20000000000
(newline)
(display (number->string -2147483648 10)) ; -2147483648
(newline)
(display (number->string -2147483648 16)) ; -80000000
(newline)
(display (number->string -2147483647)) ; -2147483647
(newline)
(display (number->string -2147483647 2)) ; -1111111111111111111111111111111
(newline)
(display (number->string -2147483647 8)) ; -17777777777
(newline)
(display (number->string -2147483647 10)) ; -2147483647
(newline)
(display (number->string -2147483647 16)) ; -7fffffff
(newline)
(display (number->string #x8000ffff 10)) ; -2147418113
(newline)
