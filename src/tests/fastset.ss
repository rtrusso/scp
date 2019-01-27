(need sasm/fastset)
(need util/list)

(define foo '(a b c d e f g h i j k l m n o p q r s t u v w x y z))
(define imp (fastset-importer foo))
(define exp (fastset-exporter imp foo))
(define (test-imp x)
  (fastset-collapse imp x))
(define (test-exp x)
  (fastset-expand exp x))

(if (not (equal? foo (test-exp (test-imp foo))))
    (error "so many problems in the world"))
