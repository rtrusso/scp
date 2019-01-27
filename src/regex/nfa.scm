(need util/list)
(need util/string)
(need regex/fsm)
(need regex/dfa)

;; NFA Rule interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nfa-rule from value to)
  (vector from value to))
(define (nfa-lambda-rule from to)
  (vector from to))
(define (nfa-rule.lambda? nfa-rule)
  (and (vector? nfa-rule) (= 2 (vector-length nfa-rule))))
(define (nfa-rule.from nfa-rule)
  (vector-ref nfa-rule 0))
(define (nfa-rule.from? nfa-rule s)
  (state=? s (nfa-rule.from nfa-rule)))
(define (nfa-rule.to nfa-rule)
  (cond ((nfa-rule.lambda? nfa-rule) (vector-ref nfa-rule 1))
        (else (vector-ref nfa-rule 2))))
(define (nfa-rule.value nfa-rule)
  (if (nfa-rule.lambda? nfa-rule)
      (error "Lambda (empty-string) rule has no value -- NFA-RULE.VALUE"
             nfa-rule))
  (vector-ref nfa-rule 1))
(define (nfa-rule.value? nfa-rule value)
  (or (equal? (nfa-rule.value nfa-rule) value)
      (and (string? (nfa-rule.value nfa-rule))
           (char? value)
           (strchr (nfa-rule.value nfa-rule) value))))

(define (nfa-rule-filter p? nfa-rules)
  (remove-duplicate-states (map nfa-rule.to (filter p? nfa-rules))))

;; NFA interface ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nfa? obj)
  (tagged? 'nfa obj))

(define (nfa.current-states nfa)
  ((safe-contents 'nfa nfa) 'get-current))
(define (nfa.emit-rule-set nfa)
  ((safe-contents 'nfa nfa) 'get-emit-rules))
(define (nfa.current-emitted-values nfa)
  (let ((result '()))
    ((safe-contents 'nfa nfa) 'emit
     (lambda (x) (set! result (cons x result))))
    result))
(define (nfa.get-emitted-values-for-states nfa states)
  (let ((result '())
        (rules (nfa.emit-rule-set nfa)))
    (for-each (lambda (state)
                (let ((item (assoc state rules)))
                  (if (and item
                           (not (member item result)))
                      (set! result (cons (cdr item) result)))))
              states)
    result))
(define (nfa.initial-state nfa)
  ((safe-contents 'nfa nfa) 'get-initial))
(define (nfa.final-states nfa)
  ((safe-contents 'nfa nfa) 'get-final))
(define (nfa.rule-set nfa)
  ((safe-contents 'nfa nfa) 'get-rules))
(define (nfa.input! nfa obj)
  ((safe-contents 'nfa nfa) 'input obj))
(define (nfa.reset! nfa)
  ((safe-contents 'nfa nfa) 'reset))
(define (nfa.failed? nfa)
  (null? (nfa.current-states nfa)))
(define (nfa.done? nfa)
  (and (not (nfa.failed? nfa))
       (let ((isect
              (intersect-lists state=?
                               (nfa.current-states nfa)
                               (nfa.final-states nfa))))
         (and (not (null? isect)) isect))))

;; Advanced NFA routines ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nfa.reachable-states nfa)
  (depth-first-search state=?
                      (nfa.initial-state nfa)
                      (lambda (state)
                        (map nfa-rule.to
                             (filter (lambda (rule)
                                       (nfa-rule.from? rule state))
                                     (nfa.rule-set nfa))))))

(define (nfa.referenced-states nfa)
  (remove-duplicate-states
   (append (cons (nfa.initial-state nfa)
                 (nfa.final-states nfa))
           (map nfa-rule.from (nfa.rule-set nfa))
           (map nfa-rule.to (nfa.rule-set nfa)))))

(define (nfa:possible-transitions nfa state)
  (remove-duplicates
   equal?
   (append-map
    (lambda (rule)
      (let ((val (nfa-rule.value rule)))
        (cond ((string? val)
               (string->list val))
              ((char? val)
               (list val))
              (else
               (error "Unrecognized NFA rule value: " val)))))
    (filter (lambda (rule) 
              (and (not (nfa-rule.lambda? rule))
                   (nfa-rule.from? rule state)))
            (nfa.rule-set nfa)))))

(define (nfa:possible-transitions* nfa statelist)
  (remove-duplicates
   equal?
   (append-map (lambda (state) (nfa:possible-transitions nfa state))
	       statelist)))



(define (nfa:lambda-closure nfa state)
  (depth-first-search state=?
		      state
		      (lambda (state)
			(remove-duplicate-states
			 (nfa-rule-filter (lambda (rule)
					    (and (nfa-rule.lambda? rule)
						 (nfa-rule.from? rule state)))
					  (nfa.rule-set nfa))))))

(define (nfa:next nfa state obj)
  (define (matching-rule? rule)
    (and (nfa-rule.from? rule state)
	 (not (nfa-rule.lambda? rule))
	 (nfa-rule.value? rule obj)))
  (remove-duplicate-states
   (append-map (lambda (state) (nfa:lambda-closure nfa state))
	       (nfa-rule-filter matching-rule? (nfa.rule-set nfa)))))

(define (nfa:next* nfa states obj)
  (remove-duplicate-states
   (append-map (lambda (state) (nfa:lambda-closure nfa state))
	       (append-map (lambda (state) (nfa:next nfa state obj))
			   states))))

;; NFA constructor ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (build-nfa initial-state final-states rules emit-rules)
  (define (lambda-closure state)
    (depth-first-search state=?
                        state
                        (lambda (state)
                          (remove-duplicate-states
                           (nfa-rule-filter (lambda (rule)
                                              (and (nfa-rule.lambda? rule)
                                                   (nfa-rule.from? rule state)))
                                            rules)))))
  (let ((cur (lambda-closure initial-state)))
    (define (get-current)
      cur)
    (define (reset)
      (set! cur (lambda-closure initial-state)))
    (define (input obj)
      (define (next state)
        (define (matching-rule? rule)
          (and (nfa-rule.from? rule state)
               (not (nfa-rule.lambda? rule))
               (nfa-rule.value? rule obj)))
        (nfa-rule-filter matching-rule? rules))
      (set! cur 
            (remove-duplicate-states 
             (append-map lambda-closure (append-map next cur))))
      cur)
    (define (get-initial)
      initial-state)
    (define (get-final)
      final-states)
    (define (get-rules)
      rules)
    (define (get-emit-rules)
      emit-rules)
    (define (emit receive-result)
      (for-each (lambda (state)
                  (let ((result (assoc state emit-rules)))
                    (if result
                        (receive-result (cdr result)))))
                (get-current)))
    (define (nfa-dispatch sym . args)
      (apply (case sym
               ((get-current) get-current)
               ((get-initial) get-initial)
               ((get-final) get-final)
               ((input) input)
               ((reset) reset)
               ((emit) emit)
               ((get-emit-rules) get-emit-rules)
               ((get-rules) get-rules))
             args))
    (tag 'nfa nfa-dispatch)))

;; NFA transforms ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nfa->dfa nfa)
  (define unique-state (let ((s -1)) (lambda () (set! s (+ s 1)) s)))

  (define (possible-transitions* states)
    (nfa:possible-transitions* nfa states))

  (define (dfa-transformer nfa-states)
    (lambda (state) (index-of state
			      (list-set-comparator equal?)
			      nfa-states)))

  (define (compute-final-states statelists)
    (remove-duplicates
     (list-set-comparator state=?)
     (filter (lambda (statelist)
	       (not (null? (intersect-lists state=?
					    statelist
					    (nfa.final-states nfa)))))
	     statelists)))

  (define (create-dfa visited rules)
    (dfa-transform
     (build-dfa (nfa:lambda-closure nfa (nfa.initial-state nfa))
                (compute-final-states visited)
                rules
                (filter-map
                 (lambda (statelist)
                   (let ((emit
                          (nfa.get-emitted-values-for-states nfa statelist)))
                     (and (not (null? emit))
                          (cons statelist emit))))
                 visited))
     (dfa-transformer visited))
    )

  (define (iter queue visited rules)

    (define (statelists-to-queue cur-statelist)
      (map (lambda (obj)
	     (nfa:next* nfa cur-statelist obj))
	   (possible-transitions* cur-statelist)))

    (define (new-rules cur-statelist)
      (map 
       (lambda (obj)
	 (nfa-rule cur-statelist obj (nfa:next* nfa cur-statelist obj)))
       (possible-transitions* cur-statelist)))

    (define (visited? statelist)
      (list-contains? (list-set-comparator state=?)
		      statelist
		      visited))

    (define (queue-statelists list-of-statelists)
      (union-lists (list-set-comparator state=?)
		   (subtract-lists (list-set-comparator state=?)
				   list-of-statelists
				   visited)
		   (cdr queue)))

    (define (queue-rules new-rules)
      (append new-rules rules))

    (if (null? queue)
	(create-dfa visited rules)
	(let ((cur-statelist (car queue)))
;(display "Current: ") (write cur-statelist) (newline)
;(display "Queue: ") (write queue) (newline)
;(display "Visited: ") (write visited) (newline)
;(display "Rules: ") (write rules) (newline)
;(newline)
;(newline)
;(newline)
	  (iter (queue-statelists (statelists-to-queue cur-statelist))
		(cons cur-statelist visited)
		(queue-rules (new-rules cur-statelist))))))
  (iter (list (nfa:lambda-closure nfa (nfa.initial-state nfa)))
	'()
	'()))

;; (define 
;;   nex (build-nfa #\a (list #\b)
;;                  (list (nfa-rule #\a 3 #\b)
;;                        (nfa-rule #\a 3 #\c)
;;                        (nfa-lambda-rule #\a #\b))
;;                  '((#\a . 0)
;;                    (#\b . 1))))

;; NFA graphviz integration ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (nfa.graphviz nfa)
  (display "digraph nondeterministic_finite_automaton {\n")
  (display "    rankdir=LR;\n")
  (begin (display "    node [shape = doublecircle]; ")
         (display (nfa.initial-state nfa))
         (display ";\n"))
  (display "    node [shape = circle];\n")
  (for-each (lambda (rule)
              (display "    ")
              (display (nfa-rule.from rule))
              (display " -> ")
              (display (nfa-rule.to rule))
              (display " [ label = ")
              (if (nfa-rule.lambda? rule)
                  (write "***")
                  (cond ((char? (nfa-rule.value rule))
                         (display #\")
                         (write (nfa-rule.value rule))
                         (display #\"))
                        (else
                         (write (nfa-rule.value rule)))))
              (display " ];\n"))
            (nfa.rule-set nfa))
  (display "}\n"))

;; Generic Procedure setup ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(genproc-install 'fsm? '(nfa) nfa?)
(genproc-install 'fsm.input! '(nfa <character>) nfa.input!)
(genproc-install 'fsm.reset! '(nfa) nfa.reset!)
(genproc-install 'fsm.done? '(nfa) nfa.done?)
(genproc-install 'fsm.failed? '(nfa) nfa.failed?)
