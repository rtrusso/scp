(need algo/bsearch)

(define (build-fast-dfa start-state end-array adjacency-matrix emit-vector)
  (define bs
    (make-binary-searcher vector-ref vector-length <))
  (define (char-small-enough? ch-as-int)
    (<= 0 ch-as-int 127))
  (define (lookup-rule rule ch-as-int)
    (if (char-small-enough? ch-as-int)
        (vector-ref (vector-ref rule 0) ch-as-int)
        (let ((index (bs (vector-ref rule 1) ch-as-int)))
          (and index (vector-ref (vector-ref rule 2) index)))))
  (define (false-to-minus-1 l)
    (map (lambda (x) (if x x -1)) l))
  (define (run-length-encode int-list)
    (define (add-entry current-count current result)
      (if (= 0 current-count)
	  result
	  (append result (list current-count current))))
    (define (iter current-count current result rest)
      (cond ((null? rest)
	     (add-entry current-count current result))
	    ((= (car rest) current)
	     (iter (+ 1 current-count) current result (cdr rest)))
	    (else
	     (iter 1
		   (car rest)
		   (add-entry current-count current result)
		   (cdr rest)))))
    (if (null? int-list)
	int-list
	(iter 1 (car int-list) '() (cdr int-list))))
  (define (char-as-java-string char)
    (cond ((char=? char #\')
           "'\\''")
          ((char=? char #\\)
           "'\\\\'")
          ((member char (string->list "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789!@#$%^&*()-_=+[]{}|\";:,.<>/?"))
           (string-append "'" (make-string 1 char) "'"))
          (else
           (string-append "((char)" (number->string (char->integer char)) ")"))))
  (let*
      ((current-state start-state)
       (current-rule (vector-ref adjacency-matrix start-state))
       (vtab (vector
              ;; reset! - method 0
              (lambda (dont-care)
                (set! current-state start-state)
                (set! current-rule (vector-ref adjacency-matrix
                                               current-state)))
              ;; input! - method 1
              (lambda (ch)
                (if current-state
                    (begin (set! current-state
                                 (lookup-rule current-rule
                                              (char->integer ch)))
                           (if current-state
                               (set! current-rule
                                     (vector-ref adjacency-matrix
                                                 current-state))
                               (set! current-rule '())))))
              ;; done? - method 2
              (lambda (dont-care)
                (and current-state
                     (vector-ref end-array current-state)))
              ;; failed? - method 3
              (lambda (dont-care)
                (not current-state))
              ;; serialize - method 4
              (lambda (dont-care)
                `(build-fast-dfa (quote ,start-state)
                                 (quote ,end-array)
                                 (quote ,adjacency-matrix)
                                 (quote ,emit-vector)))
              ;; validate - method 5
              (lambda (dont-care)
                (define (err . args)
                  (for-each display args)
                  (newline)
                  #f)
                (and (< -1 start-state (vector-length end-array))
                     (= (vector-length end-array) (vector-length adjacency-matrix))
                     (accum-map
                      (lambda (a b) (and a b))
                      #t
                      (lambda (rule)
                        (and 
                         (or (= 3 (vector-length rule))
                             (err "rule does not contain 3 entries"))
                         (or (= 128 (vector-length (vector-ref rule 0)))
                             (err "txn-table is not of length 128 " rule))
                         (accum-map
                          (lambda (a b) (and a b))
                          #t
                          (lambda (x) (or (or (not x) (< -1 x (vector-length end-array)))
                                          (err "txn-table specifies invalid target " x " " rule)))
                          (vector->list (vector-ref rule 0)))
                         (or (let ((chars (vector->list (vector-ref rule 1))))
                               (or (< (length chars) 2)
                                   (apply < chars)))
                             (err "transition entries are not sorted " rule))
                         (or (accum-map (lambda (a b) (and a b))
                                        #t
                                        (lambda (x) (not (char-small-enough? x)))
                                        (vector->list (vector-ref rule 1)))
                             (err "transition entries contain invalid characters " rule))
                         (or (accum-map (lambda (a b) (and a b))
                                        #t
                                        (lambda (x) (or (< -1 x (vector-length end-array))
                                                        (err "transition entries contain invalid states "
                                                             rule)))
                                        (vector->list (vector-ref rule 1)) ))))
                      (vector->list adjacency-matrix))))
              ;; emit - method 6
              (lambda (dont-care)
                (vector-ref emit-vector current-state))
              ;; build java dfa - method 7
              (lambda (package import class-decoration class-name port)
                (if package
                    (display (string-append "package " package ";\n\n") port))
                (if import
                    (display (string-append "import " import ";\n\n") port))
                (display (string-append (if class-decoration
                                            (string-append class-decoration
                                                           " ")
                                            "")
                                        "class "
                                        class-name
                                        " {\n") port)
                (display "\n" port)
                ;; start state
                (display (string-append "    public static final int startState = " 
                                        (number->string start-state)
                                        ";\n")
                         port)
                (display "\n\n" port)
                ;; end state map
                (display (string-append "    public static final boolean[] endStateMap = new boolean[] {\n") port)
                (for-each (lambda (end?)
                            (display (string-append "        "
                                                    (if end? "true" "false")
                                                    ",\n")
                                     port))
                          (vector->list end-array))
                (display "    };\n" port)
                (display "\n\n" port)
                ;; adjacency matrix
                (display "    public static AdjacencyRule[] getAdjacencyRules() {\n" port)
                (display "        return new AdjacencyRule[] {\n" port)
                (for-each (lambda (adjacency-rule)
                            (let ((adjacency-table (vector-ref adjacency-rule 0))
                                  (char-table (vector-ref adjacency-rule 1))
                                  (char-map (vector-ref adjacency-rule 2)))
                              (display "        new AdjacencyRule(\n" port)
                              (display (string-append "            new int[] {"
                                                      (apply string-append
                                                             (map (lambda (x)
                                                                    (string-append (number->string x) ", "))
                                                                  (run-length-encode (false-to-minus-1 (vector->list adjacency-table)))))
						      "}")
                                       port)
			      (if (not (zero? (vector-length char-map)))
				  (begin
				    (display ",\n" port)
				    (display (string-append "            new char[] {"
							    (apply string-append
								   (map (lambda (x)
									  (string-append (char-as-java-string x) ", "))
									(vector->list char-table)))
							    "},\n")
					     port)
				    (display (string-append "            new int[] {"
							    (apply string-append
								   (map (lambda (x)
									  (string-append (if x (number->string x) "-1") ", "))
									(vector->list char-map)))
							    "}")
					     port)))
			      (display "\n        ),\n\n" port)))
			    (vector->list adjacency-matrix))
                (display "        };\n" port)
                (display "    }\n" port)
                (display "\n\n" port)
                ;; emit vector
                (display "    public static final String[] emitMap = new String[] {\n" port)
                (for-each (lambda (entry)
                            (display "        " port)
                            (if (symbol? entry)
                                (write (symbol->string entry) port)
                                (display "null" port))
                            (display ",\n" port))
                          (vector->list emit-vector))
                (display "    };\n" port)
                (display "\n\n" port)
                ;; end class
                (display "}\n" port))
              )) ;; vtab
       ) ;; let
    (lambda (method . args)
      (apply (vector-ref vtab method) args)))) ;; build-fast-dfa

(define (fast-dfa-reset! fast-dfa)
  (fast-dfa 0 '()))

(define (fast-dfa-input! fast-dfa char)
  (fast-dfa 1 char))

(define (fast-dfa-done? fast-dfa)
  (fast-dfa 2 '()))

(define (fast-dfa-failed? fast-dfa)
  (fast-dfa 3 '()))

(define (fast-dfa-serialize fast-dfa)
  (fast-dfa 4 '()))

(define (fast-dfa-validate fast-dfa)
  (fast-dfa 5 '()))

(define (fast-dfa-emit fast-dfa)
  (fast-dfa 6 '()))

(define (fast-dfa-serialize-java fast-dfa
                                 package
                                 import
                                 class-decoration
                                 class-name
                                 port)
  (fast-dfa 7 package import class-decoration class-name port))
