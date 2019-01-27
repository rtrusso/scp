;; nonexports.scm
;;
;; A quick and dirty program to print out the non-exported functions
;; in a SASM file.  It prints these out in (export) statements so that
;; they can be easily cut-and-pasted pasted into another sasm file.
;;
;; Run like so: mzscheme --script nonexports.scm <foo.sasm

(need util/io)
(need util/list)

(let ((stmts (read-fully (current-input-port))))
  (let ((exports (filter-map (lambda (stmt)
                               (and (eqv? 'export (car stmt))
                                    (cadr stmt)))
                             stmts))
        (functions (filter-map (lambda (stmt)
                                 (and (eqv? 'function (car stmt))
                                      (cadr (assoc 'name (cdr stmt)))))
                               stmts)))
    (for-each (lambda (x) (display `(export ,x)) (newline))
              (subtract-lists eqv? functions exports))))
