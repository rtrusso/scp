(need util/list)

(define (println . args)
  (for-each display args)
  (newline))

;; productions ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (production-lhs production)
  (car production))

(define (production-rhs production)
  (cdr production))

(define (make-production-item prod item)
  (cons prod item))

(define (production-initial-item prod)
  (make-production-item prod (production-rhs prod)))

(define (production->items prod)
  (define (iter rhs-rest items)
    (if (null? rhs-rest)
        (reverse (cons (make-production-item prod rhs-rest)
                       items))
        (iter (cdr rhs-rest)
              (cons (make-production-item prod rhs-rest)
                    items))))
  (iter (production-rhs prod) '()))

;; items ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (item-production item)
  (car item))

(define (item-position item)
  (cdr item))

(define (item-first item)
  (cadr item))

(define (item-transition item)
  (make-production-item (item-production item)
                        (cdr (item-position item))))

(define (item-done? item)
  (null? (item-position item)))

(define (items-equal? item-a item-b)
  (equal? item-a item-b))

(define (print-item item)
  (define (print-rhs rhs)
    (if (equal? rhs (item-position item))
        (display " ."))
    (if (not (null? rhs))
        (begin (display " ")
               (display (car rhs))
               (print-rhs (cdr rhs)))))
  (display (production-lhs (item-production item)))
  (display " ->")
  (print-rhs (production-rhs (item-production item))))

;; grammars ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (grammar-start-production grammar)
  (car grammar))

(define (grammar-productions grammar)
  grammar)

(define (grammar-nonterminals grammar)
  (remove-duplicates equal?
                     (map car grammar)))

(define (grammar-symbols grammar)
  (remove-duplicates equal?
                     (accum append '() (grammar-productions grammar))))

(define (grammar-terminals grammar)
  (subtract-lists equal? 
                  (grammar-symbols grammar)
                  (grammar-nonterminals grammar)))

(define (nonterminal? grammar symbol)
  (member symbol (grammar-nonterminals grammar)))

(define (productions-starting-with grammar symbol)
  (filter (lambda (prod) (eqv? symbol (production-lhs prod)))
          (grammar-productions grammar)))

(define (productions-containing grammar symbol)
  (filter (lambda (prod) (member symbol (production-rhs prod)))
          (grammar-productions grammar)))

(define (grammar-nullable-set grammar)
  (define (find-not-nullable not-nullable symbols)
    (if (null? symbols)
        #f
        (let ((prods (productions-starting-with grammar (car symbols))))
          (if (not (null? (filter (lambda (prod) (intersect-lists equal? 
                                                                  (production-rhs prod)
                                                                  not-nullable))
                                  (productions-starting-with grammar (car symbols)))))
              (car symbols)
              (find-not-nullable not-nullable (cdr symbols))))))
  (define (find-nullable nullable symbols)
    (if (null? symbols)
        #f
        (let ((prods (productions-starting-with grammar (car symbols))))
          (if (accum (lambda (a b) (and a b)) 
                     #t 
                     (map (lambda (prod) (list-subset? equal? (production-rhs prod) nullable ))
                          prods))
              (car symbols)
              (find-nullable nullable (cdr symbols))))))
  (define (without l sym)
    (filter (lambda (x) (not (equal? x sym)))
            l))
  (define (iter not-nullable nullable to-check)
    (if (null? to-check)
        nullable
        (let ((new-not-nullable (find-not-nullable not-nullable to-check)))
          (if new-not-nullable
              (iter (cons new-not-nullable not-nullable)
                    nullable
                    (without to-check new-not-nullable))
              (let ((new-nullable (find-nullable nullable to-check)))
                (if new-nullable
                    (iter not-nullable
                          (cons new-nullable nullable)
                          (without to-check nullable))
                    nullable))))))
  (iter (grammar-terminals grammar) '() (grammar-nonterminals grammar)))

(define (grammar-first-sets grammar)
  (define (add-first l sym val)
    (if (null? l)
        (list (cons sym val))
        (if (equal? sym (caar l))
            (cons (cons (caar l)
                        (union-lists equal? val (cdar l)))
                  (cdr l))
            (cons (car l) (add-first (cdr l) sym val)))))
  (define (get-first first sym)
    (let ((e (assoc sym first)))
      (if e (cdr e) '())))
  (define (prod-first prod)
    (car (production-rhs prod)))
  (define (prod-useful? first prod)
    (not (list-subset? equal? 
                       (get-first first (prod-first prod))
                       (get-first first (production-lhs prod)))))
  (define (iter first all-prods)
    (let loop ((prods all-prods))
      (if (null? prods)
          first
          (let ((prod (car prods)))
            (if (and (not (null? (production-rhs prod)))
                     (not (null? (get-first first (prod-first prod))))
                     (prod-useful? first prod))
                (iter (add-first first
                                 (production-lhs prod)
                                 (get-first first (prod-first prod)))
                      all-prods)
                (loop (cdr prods)))))))
  (iter (map (lambda (x) (list x x))
             (grammar-terminals grammar))
        (grammar-productions grammar)))

(define (grammar-follow-sets grammar)
  (define first-sets (cons (list '$ '$) (grammar-first-sets grammar)))
  (define (get-first first sym)
    (let ((e (assoc sym first)))
      (if e (cdr e) '())))
  (define (add-follow l sym val)
    (if (null? l)
        (list (cons sym val))
        (if (equal? sym (caar l))
            (cons (cons (caar l)
                        (union-lists equal? val (cdar l)))
                  (cdr l))
            (cons (car l) (add-follow (cdr l) sym val)))))
  (define (get-follow follow sym)
    (let ((e (assoc sym follow)))
      (if e (cdr e) '())))
  (define (iter follow all-prods)
    (let loop ((prods all-prods))
      (if (null? prods)
          follow
          (let ((prod (car prods)))
            (let prod-loop ((items (production-rhs prod)))
              (cond ((null? items)
                     (loop (cdr prods)))
                    ((null? (cdr items))
                     (if (and (nonterminal? grammar (car items))
                              (not (null? (get-follow follow (production-lhs prod))))
                              (not (list-subset? equal?
                                                 (get-follow follow (production-lhs prod))
                                                 (get-follow follow (car items)))))
                         (iter (add-follow follow
                                           (car items)
                                           (get-follow follow (production-lhs prod)))
                               all-prods)
                         (loop (cdr prods))))
                    (else
                     (if (and (nonterminal? grammar (car items))
                              (not (null? (get-first first-sets (cadr items))))
                              (not (list-subset? equal? 
                                                 (get-first first-sets (cadr items))
                                                 (get-follow follow (car items)))))
                         (iter (add-follow follow
                                           (car items)
                                           (get-first first-sets (cadr items)))
                               all-prods)
                         (prod-loop (cdr items))))))))))
  (iter (list (cons (production-lhs (grammar-start-production grammar))
                    (list '$)))
        (grammar-productions grammar)))

(define (grammar-rules-filter-productions grammar-rules)
  (filter (lambda (x) (not (member (car x) '(:rule :java-rule))))
          grammar-rules))

(define (grammar-rules-build-action-table grammar-rules)
  (let ((actions (filter (lambda (x) (member (car x) '(:rule))) grammar-rules)))
    (if (= (length (grammar-rules-filter-productions grammar-rules))
           (length actions))
        (cons 'vector (map (lambda (x)
                             (if (and (list? x)
                                      (= 2 (length x))
                                      (eqv? 'lambda (car (list-ref x 1))))
                                 (list-ref x 1)
                                 (error "Invalid action: " x)))
                           actions))
        (error "The number of productions does not equal the number of rules"
               (length (grammar-rules-filter-productions grammar-rules))
               (length actions)))))

;; LR(0) generator ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (state-from-item-list-closure grammar items)
  (define (add-item item result)
    (if (not (member item result))
        (cons item result)
        result))
  (define (add-items items visited result)
    (if (null? items)
        result
        (if (member (car items) visited)
            (add-items (cdr items) visited result)
            (add-item (car items) (add-items (cdr items) visited result)))))
  (define (add-nonvisited item visited result)
    (if (member item visited)
        result
        (cons item result)))
  (define (iter items visited result)
    (if (null? items) 
        (reverse result)
        (let ((item (car items))
              (new-visited (add-item (car items) visited)))
          (cond ((item-done? item) 
                 (iter (cdr items) new-visited (add-item item result)))
                ((not (nonterminal? grammar (item-first item)))
                 (iter (cdr items) new-visited (add-item item result)))
                (else
                 (iter (add-items (map production-initial-item
                                       (productions-starting-with grammar (item-first item)))
                                  new-visited
                                  (cdr items))
                       new-visited
                       (add-item item result)))))))
  (iter items '() '()))

(define (state-from-item-closure grammar item)
  (state-from-item-list-closure grammar (list item)))

(define (state-items state)
  state)

(define (state-transition-symbols state)
  (remove-duplicates equal?
                     (map item-first
                          (filter (lambda (i) (not (item-done? i)))
                                  (state-items state)))))

(define (state-transition-items state symbol)
  (filter (lambda (item)
            (and (not (item-done? item))
                 (equal? (item-first item) symbol)))
          (state-items state)))

(define (build-lr0-state-transition-graph grammar)
  (define (states-equal? state-a state-b)
    (list-set=? items-equal? (state-items state-a) (state-items state-b)))
  (define (build-graph queue states transitions)
    (if (null? queue)
        (list states transitions)
        (let loop ((state (car queue))
                   (state-idx (index-of (car queue) states-equal? states))
                   (next-queue (cdr queue))
                   (next-states states)
                   (next-transitions transitions)
                   (next-symbols (state-transition-symbols (car queue))))
          (if (null? next-symbols)
              (build-graph next-queue next-states next-transitions)
              (let* ((next-items (state-transition-items state 
                                                         (car next-symbols)))
                     (next-state (state-from-item-list-closure grammar 
                                                               (map item-transition next-items)))
                     (next-idx (index-of next-state states-equal? next-states)))
                (if next-idx
                    (loop state
                          state-idx
                          next-queue
                          next-states
                          (append next-transitions 
                                  (list (list state-idx next-idx (car next-symbols))))
                          (cdr next-symbols))
                    (loop state
                          state-idx
                          (append next-queue (list next-state))
                          (append next-states (list next-state))
                          (append next-transitions 
                                  (list (list state-idx (length next-states) (car next-symbols))))
                          (cdr next-symbols))))))))
  (let ((start-prod (grammar-start-production grammar)))
    (let ((start-item (production-initial-item start-prod)))
      (let ((start-state (state-from-item-closure grammar start-item)))
        (build-graph (list start-state)
                     (list start-state)
                     '())))))

(define (lr0-graph-states graph)
  (car graph))

(define (lr0-graph-transitions graph)
  (cadr graph))

(define (grammar-make-table n-rows n-columns)
  (let ((rows (make-vector n-rows)))
    (let loop ((row 0))
      (if (< row n-rows)
          (begin (vector-set! rows row (make-vector n-columns #f))
                 (loop (+ row 1)))
          rows))))

(define (grammar-table-set! tab row col val)
  (vector-set! (vector-ref tab row)
               col val))

(define (grammar-table-get tab row col)
  (vector-ref (vector-ref tab row)
              col))

(define (generate-lr0-goto-table grammar graph)
  (let ((states (lr0-graph-states graph))
        (nonterm (grammar-nonterminals grammar)))
    (let ((sr-table (grammar-make-table (length states) (length nonterm))))
      (for-each (lambda (tx)
                  (let ((sym (list-ref tx 2))
                        (orig-state (list-ref tx 0))
                        (next-state (list-ref tx 1)))
                    (if (and (member sym nonterm)
                             (grammar-table-get sr-table orig-state (index-of sym equal? nonterm)))
                        (error "Goto conflict " orig-state " " next-state " " sym))
                    (if (member sym nonterm)
                        (grammar-table-set! sr-table
                                            orig-state
                                            (index-of sym equal? nonterm)
                                            (cons 'g next-state)))))
                (lr0-graph-transitions graph))
      sr-table)))

(define (generate-lr0-table grammar graph grammar-rules)
  (let ((n-states (length (lr0-graph-states graph)))
        (term (append (grammar-terminals grammar) (list '$)))
        (nonterm (grammar-nonterminals grammar))
        (follow-sets (grammar-follow-sets grammar)))
    (define (reduce-reduce-error state-idx orig-rule-idx new-rule-idx)
      (error "Reduce-reduce conflict in state " state-idx
             " between rule " orig-rule-idx " and " new-rule-idx))
    (define (term? sym) (member sym term))
    (define (term-idx sym) (index-of sym equal? term))
    (define (accept-item? item)
      (and (equal? (grammar-start-production grammar)
                   (item-production item))
           (item-done? item)))
    (define (accept-state? state)
      (accum (lambda (a b) (or a b))
             #f
             (map accept-item? (state-items state))))
    (let ((sr-table (grammar-make-table n-states (length term))))
      (define (get-conflicts row reduce-on)
        (let loop ((row (vector->list (vector-ref sr-table row)))
                   (terms term)
                   (res '()))
          (if (null? row)
              (filter (lambda (x) (not (equal? '$ (car x))))
                      (reverse res))
              (loop (cdr row)
                    (cdr terms)
                    (if (pair? (car row))
                        (if (or (null? reduce-on)
                                (member (car terms) reduce-on))
                            (cons (cons (car terms) (cdar row)) res)
                            res)
                        res)))))
      (for-each (lambda (tx)
                  (if (term? (list-ref tx 2))
                      (grammar-table-set! sr-table 
                                          (car tx)
                                          (term-idx (list-ref tx 2))
                                          (cons 's (cadr tx)))))
                (lr0-graph-transitions graph))
      (for-each-idx (lambda (state idx)
                      (if (accept-state? state)
                          (grammar-table-set! sr-table
                                              idx
                                              (term-idx '$)
                                              (cons 'a #f))))
                    (lr0-graph-states graph))
      (let loop ((states (lr0-graph-states graph))
                 (state-idx 0))
        (if (< state-idx n-states)
            (begin
              (for-each (lambda (item)
                          (if (item-done? item)
                              (let ((conflicts (get-conflicts 
                                                state-idx
                                                (let ((e (assoc (production-lhs (item-production item))
                                                                follow-sets)))
                                                  (if e (cdr e) '())))))
                                (if (not (null? conflicts))

                                    ;; Conflicts exist, bail out
                                    (begin 
                                      (println "In state " state-idx "; rule " (item-production item)
                                               " conflicts with shift states:")
                                      (for-each (lambda (x) (println "   " x)) conflicts))

                                    ;; No conflicts
                                    (let ((follow (assoc (production-lhs (item-production item))
                                                         follow-sets)))
                                      (if (not follow)
                                          (error "No follow set for production " (item-production item)
                                                 " in state " state-idx))
                                      (for-each (lambda (term)
                                                  (let ((rule-idx (index-of (item-production item)
                                                                            equal?
                                                                            (grammar-productions grammar)))
                                                        (existing (grammar-table-get sr-table state-idx (term-idx term))))
                                                    (cond ((and existing
                                                                (eqv? 'a (car existing)))
                                                           #t)
                                                          (existing
                                                           (reduce-reduce-error state-idx (cdr existing) rule-idx))
                                                          (else
                                                           (grammar-table-set! sr-table
                                                                               state-idx
                                                                               (term-idx term)
                                                                               (cons 'r rule-idx))))))
                                                (cdr follow)))))))

                        (state-items (car states)))
              (loop (cdr states) (+ state-idx 1)))))
      sr-table)))

(define (print-state state prefix)
  (for-each (lambda (item)
              (display prefix)
              (print-item item)
              (newline))
            (state-items state)))

(define (print-state-transition-graph graph)
  (define (justify str width suffix)
    (if (< (string-length str) width)
        (justify (string-append str suffix) width suffix)
        str))
  (let loop ((states (list-ref graph 0))
             (count 0)
             (transitions (list-ref graph 1)))
    (if (not (null? states))
        (begin 
          (display (justify (string-append "+- State " (number->string count) " -") 50 "-"))
          (newline)
          (print-state (car states) "| ")
          (display (justify "+-" 50 "-"))
          (newline)
          (newline)
          (let loop ((t transitions)
                     (found #f))
            (if (not (null? t))
                (if (equal? count (car (car t)))
                    (let ((tx (car t)))
                      (display "State ")
                      (display (car tx))
                      (display " -> State ")
                      (display (cadr tx))
                      (display " on ")
                      (display (caddr tx))
                      (newline)
                      (loop (cdr t) #t))
                    (loop (cdr t) found))
                (if found (newline))))
          (loop (cdr states) (+ count 1) transitions)))))

