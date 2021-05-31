(need sasm/machdesc)

(define (x86-32-bit-intconst x)
  (if (>= x 0)
      (number->string x 16)
      (let ((bin (string->list (number->string (- x) 2))))
        (let loop ((len (length bin))
                   (res bin))
          (if (< len 32)
              (loop (+ len 1) (cons #\0 res))
              (let ((negated (map (lambda (x) (if (char=? x #\0) 1 0)) res)))
                (let ((neg-rev (reverse negated)))
                  (let ((twos-comp
                         (let add-1 ((cur neg-rev))
                           (if (null? cur)
                               (reverse neg-rev)
                               (if (= 0 (car cur))
                                   (begin (set-car! cur 1)
                                          (reverse neg-rev))
                                   (begin (set-car! cur 0)
                                          (add-1 (cdr cur))))))))
                    (let loop2 ((res '())
                              (data twos-comp))
                      (if (null? data)
                          (list->string (reverse res))
                          (let* ((x (car data))
                                 (x2 (cdr data))
                                 (y (car x2))
                                 (y2 (cdr x2))
                                 (z (car y2))
                                 (z2 (cdr y2))
                                 (w (car z2))
                                 (w2 (cdr z2)))
                            (loop2
                             (cons (list-ref '(#\0 #\1 #\2 #\3 #\4 #\5 #\6 #\7 #\8 #\9 #\a #\b #\c #\d #\e #\f)
                                             (+ (* 8 x) (* 4 y) (* 2 z) w))
                                   res)
                             w2))))))))))))


(define (nasm-x86-data)
  (machine-description
   ;; data declaration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; directive
   (instruction (input-pattern `(data (const (,intconst? i))))
                (rewrite-rule "dd 0x~" (i x86-32-bit-intconst)))

;; directive
   (instruction (input-pattern `(data (label (,label? l))))
                (rewrite-rule "dd ~" (l label)))

   (instruction (input-pattern `(data (const (,string? str))))
                (rewrite-rule "db ~" (str nasm-string)))

   ))

