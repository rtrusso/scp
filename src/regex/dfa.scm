(need util/list)
(need util/string)
(need util/vector)
(need regex/fsm)
(need regex/fast-dfa)
(need algo/bsearch)
(need algo/quicksort)

;; DFA states ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define state=? equal?)

(define (state-in? state state-list)
  (list-contains? state=? state state-list))

(define (remove-duplicate-states l)
  (remove-duplicates state=? l))

;; DFA rules ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (rule from value to)
  (vector from value to))
(define (rule.next r)
  (vector-ref r 2))
(define (rule.from r)
  (vector-ref r 0))
(define (rule.starts-from? r s)
  (state=? (rule.from r) s))
(define (rule.value r)
  (vector-ref r 1))
(define (rule.value? r val)
  (or (equal? (rule.value r) val)
      (and (string? (rule.value r))
           (char? val)
           (strchr (rule.value r) val))))

(define (get-rule rules from val)
  (define (iter rules)
    (cond ((null? rules) #f)
          ((and (state=? from (rule.from (car rules)))
                (rule.value? (car rules) val))
           (car rules))
          (else (iter (cdr rules)))))
  (iter rules))

(define (get-next-state rules cur x)
  (let ((rule (get-rule rules cur x)))
    (if rule
        (rule.next rule)
        #f)))

;; DFA ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dfa? obj)
  (tagged? 'dfa obj))

(define (dfa.current dfa)
  ((safe-contents 'dfa dfa) 'current))
(define (dfa.initial-state dfa)
  ((safe-contents 'dfa dfa) 'start))
(define (dfa.end-states dfa)
  ((safe-contents 'dfa dfa) 'enders))
(define (dfa.done? dfa)
  ((safe-contents 'dfa dfa) 'done?))
(define (dfa.input! dfa obj)
  ((safe-contents 'dfa dfa) 'go! obj))
(define (dfa.reset! dfa)
  ((safe-contents 'dfa dfa) 'reset!))
(define (dfa.failed? dfa)
  ((safe-contents 'dfa dfa) 'failed?))
(define (dfa.rule-set dfa)
  ((safe-contents 'dfa dfa) 'rule-set))
(define (dfa.emit-rule-set dfa)
  ((safe-contents 'dfa dfa) 'emit-rule-set))
(define (dfa.get-emitted-value-for-state dfa state)
  (assoc state (dfa.emit-rule-set dfa)))
(define (dfa.current-emitted-value dfa)
  (dfa.get-emitted-value-for-state dfa (dfa.current dfa)))
(define (dfa.reachable-states dfa)
  (depth-first-search state=? (dfa.initial-state dfa)
                      (lambda (state)
                        (map rule.next (filter (lambda (r)
                                                 (rule.starts-from? r state))
                                               (dfa.rule-set dfa))))))

(define (dfa.referenced-states dfa)
  (remove-duplicates state=?
                     `(,(dfa.initial-state dfa)
                       ,@(dfa.end-states dfa)
                       ,@(map rule.from (dfa.rule-set dfa))
                       ,@(map rule.next (dfa.rule-set dfa)))))



(define (build-dfa start end rules emit-rules)
  (let* ((cur start)
         (failed #f))
    (define (get-start)
      start)
    (define (get-enders)
      end)
    (define (rule-set)
      rules)
    (define (emit-rule-set)
      emit-rules)
    (define (reset!)
      (set! cur start)
      (set! failed #f))
    (define (current)
      (if (not failed)
          cur
          (set! failed #t)))
    (define (go! x)
      (if (not failed)
          (let ((next (get-next-state rules cur x)))
            (if (not next)
                (set! failed #t)
                (begin (set! cur next) next)))
          (set! failed #t)))
    (define (done?)
      (and (not failed) (state-in? cur end)))
    (define (failed?) failed)
    (define (dfa-dispatch sym . args)
      (apply (case sym 
               ((current) current)
               ((start) get-start)
               ((enders) get-enders)
               ((go!) go!)
               ((rule-set) rule-set)
               ((emit-rule-set) emit-rule-set)
               ((failed?) failed?)
               ((done?) done?)
	       ((reset!) reset!))
             args))
    (if (not (list? end))
        (error "End must be a list of states -- DFA" start end rules))
    (if (not (list? rules))
        (error "Rules must be a list of rules -- DFA" start end rules))
    (tag 'dfa dfa-dispatch)))

;; DFA transform routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dfa-serialize dfa)
  `(build-dfa (quote ,(dfa.initial-state dfa))
              (quote ,(dfa.end-states dfa))
              (quote ,(dfa.rule-set dfa))
              (quote ,(dfa.emit-rule-set dfa))))

(define (dfa-transform d transform)
  (build-dfa (transform (dfa.initial-state d))
             (map transform (dfa.end-states d))
             (map (lambda (r)
                    (rule (transform (rule.from r))
                          (rule.value r)
                          (transform (rule.next r))))
                  (dfa.rule-set d))
             (map (lambda (r)
                    (cons (transform (car r))
                          (cdr r)))
                  (dfa.emit-rule-set d))))

(define (dfa-rename d state-a state-b)
  (define (replace x)
    (if (state=? state-a x) state-b x))
  (dfa-transform d replace))

;; (define
;;   ex (build-dfa #\a (list #\b)
;;                 (list (rule #\a 0 #\b)
;;                       (rule #\a 2 #\b)
;;                       (rule #\b 1 #\a)
;;                       (rule #\c 1 #\b)
;;                       (rule #\c 3 #\a)
;;                       (rule #\z 5 #\b)
;;                       (rule #\c 2 #\b))
;;                 '()))

;; DFA graphviz integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (dfa.graphviz dfa)
  (display "digraph deterministic_finite_automaton {\n")
  (display "    rankdir=LR;\n")
  (begin (display "    node [shape = doublecircle]; ")
         (display (dfa.initial-state dfa))
         (display ";\n"))
  (display "    node [shape = circle];\n")
  (for-each (lambda (rule)
              (display "    ")
              (display (rule.from rule))
              (display " -> ")
              (display (rule.next rule))
              (display " [ label = ")
              (cond ((char? (rule.value rule))
                     (display #\")
                     (write (rule.value rule))
                     (display #\"))
                    (else
                     (write (rule.value rule))))
              (display " ];\n"))
            (dfa.rule-set dfa))
  (display "}\n"))

;; DFA -> Fast DFA translation ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (compile-dfa-to-fast-dfa d)
  (define (char-small-enough? ch)
    (<= 0 (char->integer ch) 127))
  (let*
      ((states (quicksort! (list->vector (dfa.reachable-states d)) <=))
       (start-state (binary-search-vector states < (dfa.initial-state d)))
       (end-array (vector-map (lambda (state)
                                (not (not (member state (dfa.end-states d)))))
                              states))
       (raw-rules (remove-duplicates equal? (dfa.rule-set d)))
       (compiled-rules (vector-map
                        (lambda (state)
                          (let*
                              ((txn-table (make-vector 128 #f))
                               (large-char-rules
                                (quicksort!
                                 (list->vector
                                  (filter-map (lambda (r)
                                                (if (state=? state (rule.from r))
                                                    (if (char-small-enough? (rule.value r))
                                                        (begin
                                                          (vector-set!
                                                           txn-table
                                                           (char->integer (rule.value r))
                                                           (binary-search-vector states
                                                                                 <
                                                                                 (rule.next r)))
                                                          #f)
                                                        (cons (char->integer (rule.value r))
                                                              (binary-search-vector states
                                                                                    <
                                                                                    (rule.next r))))
                                                    #f))
                                              raw-rules))
                                 (lambda (x y) (< (car x) (car y))))))
                            (vector txn-table
                                    (vector-map car large-char-rules)
                                    (vector-map cdr large-char-rules))))
                        states))
       (emit-directive<? (lambda (x y) (< (cdr x) (cdr y))))
       (emit-directive-value car)
       (compiled-emits (vector-map (lambda (state)
                                     (let ((emit-entry
                                      (dfa.get-emitted-value-for-state d state)))
                                       (and emit-entry
                                            (if (> (length (cdr emit-entry)) 2)
                                                (let ((priority-order (quicksort-list (cdr emit-entry) emit-directive<?)))
                                                  (display "resolving emit conflict: ")
                                                  (write (map emit-directive-value priority-order))
                                                  (newline)
                                                  (emit-directive-value (car priority-order)))
                                                (emit-directive-value (cadr emit-entry))))))
                                   states)))
    (build-fast-dfa start-state end-array compiled-rules compiled-emits)))

;; Generic procedure setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(genproc-install 'fsm? '(dfa) dfa?)
(genproc-install 'fsm.input! '(dfa <character>) dfa.input!)
(genproc-install 'fsm.reset! '(dfa) dfa.reset!)
(genproc-install 'fsm.done? '(dfa) dfa.done?)
(genproc-install 'fsm.failed? '(dfa) dfa.failed?)
