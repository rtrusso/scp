(display (current-seconds))
(newline)
(display (current-milliseconds))
(newline)
(display (let loop ((i 0))
           (if (>= i 99999)
               (+ i 1)
               (loop (+ i 1)))))
(newline)
(display (current-seconds))
(newline)
(display (current-milliseconds))
(newline)
