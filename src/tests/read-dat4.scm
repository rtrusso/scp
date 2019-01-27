(define-symconst
 ;; comment line 1
 ;; comment line 2
  (sizeof:heap 8)
  (heap:base 0)
  (heap:limit 1)
  (heap:object-offset 2)
  (heap:words-per-page 3)
  (heap:shift-words-per-page 4)
  (heap:mask-words-per-page 5)
  (heap:next 6)
  (heap:type 7)

  (heap-type:fixed-alloc #xdeadface)
  (heap-type:var-alloc   #xbaadf00d)
  (negative-number -2147483639)

  ;; comment a
  (sizeof:var-heap 12)
  (var-heap:hope 9)
  (var-heap:bitmap 10)
  (var-heap:pages 11)

  ;; comment b
  (sizeof:fixed-heap 10)
  (fixed-heap:free-list 8)
  (fixed-heap:next-fixed-heap 9)
)
