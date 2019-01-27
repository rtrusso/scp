;; sasm-codegen.scm
;;
;; This module provides routines that simplify the work of generating
;; sasm code.

(define (sasm-pretty-print obj port)
  (cond
   ((and (pair? obj)
         (eqv? 'function (car obj)))
    (display "(function" port)
    (newline port)
    (for-each (lambda (part)
                (if (and (pair? part)
                         (not (eqv? 'body (car part))))
                    (begin (display "  " port)
                           (write part port)
                           (newline port))))
              obj)
    (let ((body (assoc 'body (cdr obj))))
      (if body
          (begin (display "  (body" port)
                 (newline port)
                 (for-each (lambda (insn)
                             (display "    " port)
                             (write insn port)
                             (newline port))
                           (cdr body))
                 (display "    ))" port)
                 (newline port))
          (begin 
            (display "  )" port)
            (newline port)))))
   ((and (pair? obj)
         (eqv? 'global (car obj)))
    (display "(global " port)
    (write (cadr obj) port)
    (newline port)
    (for-each (lambda (elem)
                (display "        " port)
                (write elem port)
                (newline port))
              (cddr obj))
    (display "        )" port))
   (else
    (write obj port))))
