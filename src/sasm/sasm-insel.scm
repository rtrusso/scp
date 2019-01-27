;; sasm-insel.scm
;; Instruction selection module

(need sasm/sasm-dataflow)
(need sasm/sasm-tx)

(define (sasm-temp-generator sasm-code)
  (let ((temp-max (sasm-temp-max sasm-code)))
    (lambda ()
      (set! temp-max (+ temp-max 1))
      temp-max)))

;; TODO OPTIMIZE
(define (sasm-insel sasm-code)
  (let ((liveness (sasm-liveness-compute sasm-code))
        (tempgen (sasm-temp-generator sasm-code)))
    (accum append
           '()
           (map (lambda (x y z) (sasm-insel-single x y z tempgen))
                sasm-code
                (list-ref liveness 0)
                (list-ref liveness 1)))))

(define *last-insn* '())
(define (sasm-attempt-match-insn insn)
  (let loop ((machine-spec *machine-instructions*))
    (if (null? machine-spec)
        #f
        (let ((pattern (car machine-spec)))
          (set! *last-insn* insn)
          (let ((match (pattern-match (car pattern) insn)))
            (or (and match
                     (cons match pattern))
                (loop (cdr machine-spec))))))))

(define (sasm-match-insn insn)
  (if (sasm-directive? insn)
      (error "sasm-match-insn called with directive"))
  (or (sasm-attempt-match-insn insn)
      (error "Unable to match instruction -- sasm-match-insn" insn)))

(define (sasm-directive? insn)
  (or (and (list? insn)
           (equal? 'info (car insn)))
      (pattern-match '(assign (? reg) (op side-effect))
                     insn)
      (pattern-match '(perform (op use-garbage) (? reg))
                     insn)))
;      (and (equal? 'perform (car insn))
;           (equal? '(op use-garbage) (list-ref insn 1)))))

; Some notes on instruction-selection rewrite-rules:
;
; Normal instruction, no side effects or rewrite rule
;
;    - No automatic save/restore functionality is needed
;
; If the instruction has side effects but no rewrite-rule:
;
;    - if the SE register is live-out and not modified by the current
;    instruction, save the register before and modify it after
;
;    - what about live-in?  The rewrite rule must be written carefully
;    to introduce temporaries properly
;
; If the instruction has side effects and rewrite-rule:
;
;    - if the SE register is live-in to the original instruction, save
;    it before the rewrite-rule result and restore it after the
;    rewrite-rule result


(define (sasm-insel-single insn live-in live-out tempgen)
  (define (map-side-effects side-effects)
    (if (null? side-effects)
        '()
        (append (list `(assign ,(car side-effects) (op side-effect))
                      `(perform (op use-garbage) ,(car side-effects)))
                (map-side-effects (cdr side-effects)))))
  (define (save-live-side-effect-regs insns side-effects live-in live-out)
;    (println "saving live side-effect-registers: \t\n [" insns "] \t\n[" live-in "] [" live-out "] \t\n[" side-effects "]\n\n")
    (cond ((null? side-effects)
           insns)
          ((and (member (car side-effects) live-in)
                (member (car side-effects) live-out))
           (let ((temp `(temp ,(tempgen))))
             (save-live-side-effect-regs
              (append (list `(assign ,temp ,(car side-effects)))
                      insns
                      (list `(assign ,(car side-effects) ,temp)))
              (cdr side-effects)
              live-in
              live-out)))
          (else (save-live-side-effect-regs insns (cdr side-effects) live-in live-out))))
  (define (rewrite-insn insn-pat match rewrite-spec)
    (if (not rewrite-spec)
        (list (pattern-match-replace insn-pat match))
        (let ((temps '()))
          (define (get-temp name)
            (let ((entry (assoc name temps)))
              (if entry
                  `(temp ,(cdr entry))
                  (begin (set! temps (cons (cons name (tempgen)) temps))
                         `(temp ,(cdar temps))))))
          (define (fix-statement stmt)
            (define (fix-element exp)
              (if (not (pair? exp))
                  exp
                  (case (car exp)
                    ((replace) (cdr (assoc (cadr exp) match)))
                    ((replace-temp) (get-temp (cadr exp)))
                    ((result) (fix-statement exp))
                    (else (if (pair? exp)
                              (cons (fix-element (car exp))
                                    (fix-element (cdr exp))))))))
            (map fix-element stmt))
          (filter (lambda (x) (not (pattern-match '(assign (? reg) (? reg)) x)))
                  (map fix-statement rewrite-spec)))))
  (define (write-insns insn-pat match side-effects rewrite-spec live-in live-out)
    (let* ((insns (rewrite-insn insn-pat match rewrite-spec))
           (filtered-se (filter-side-effects (list (pattern-match-replace insn-pat match))
                                             (reverse-map-side-effects side-effects)
                                             live-in
                                             live-out)))
      (let ((se-insns
             (append insns
                     (map-side-effects filtered-se))))
        (save-live-side-effect-regs se-insns
                                    filtered-se
                                    live-in
                                    live-out))))
  (define (filter-side-effects stmts side-effects live-in live-out)
    (let ((result
           (subtract-lists equal?
                           side-effects
                           (accum append '() (map sasm-liveness-statement-kills stmts)))))
;      (println "filter-side-effects [" stmts "]\n\t[" result "]")
      result))
  (define (matchrec match insn-pat side-effects rewrite-spec result live-in live-out)
    (cond ((null? match)
           (write-insns insn-pat result side-effects rewrite-spec live-in live-out))
          ((and (list? (cdr (car match)))
                (or (equal? 'result (car (cdr (car match))))
                    (equal? 'label (car (cdr (car match))))
                    (equal? 'const (car (cdr (car match))))))
           (let* ((entry (car match))
                  (result-exp (if (equal? 'result (car (cdr entry)))
                                  (cdr entry)
                                  (list 'foobar (cdr entry))))
                  (temp (tempgen))
                  (new-sub-insn `(assign (temp ,temp) ,@(cdr result-exp)))
                  (new-temp `(temp ,temp))
                  (new-match (cons (car entry) new-temp)))
             ;; (println "Matching subins " new-sub-insn "; ")
             ;; (begin (display "matchrec-foo")
             ;;        (newline)
             ;;        (display "insn: ") (write insn)
             ;;        (newline)
             ;;        (display "match: ") (write match)
             ;;        (newline)
             ;;        (display "new-sub-insn: ") (write new-sub-insn)
             ;;        (newline)
             ;;        (display "entry: ") (write entry)
             ;;        (newline)
             ;;        (display "insns-pat: ") (write insn-pat)
             ;;        (newline)
             ;;        (display "result: ") (write result)
             ;;        (newline)
             ;;        (display "side-effects: ") (write side-effects)
             ;;        (newline)
             ;;        (display "rewrite-spec: ") (write rewrite-spec)
             ;;        (newline)
             ;;        (display "live-in: ") (write live-in)
             ;;        (newline)
             ;;        (display "live-out: ") (write live-out)
             ;;        (newline)
             ;;        (newline))
             (append (sasm-insel-single new-sub-insn 
                                        live-in 
                                        (remove-duplicates equal?
                                                           (cons new-temp (append live-in live-out)))
                                        tempgen)
                     (matchrec (cdr match) insn-pat side-effects rewrite-spec (cons new-match result) (cons new-temp live-in) live-out))))
          (else (matchrec (cdr match) insn-pat side-effects rewrite-spec (cons (car match) result) live-in live-out))))
  (define (rewrite-simple-result-assignment insn)
    (if (and (equal? 'assign (car insn))
             (= 3 (length insn))
             (equal? 'result (car (list-ref insn 2))))
        `(assign ,(list-ref insn 1) ,@(cdr (list-ref insn 2)))
        insn))
;  (println "Insel-Single: " insn)
  (cond ((sasm-directive? insn)
         (list insn))
        ((sasm-directive? (rewrite-simple-result-assignment insn))
         (list (rewrite-simple-result-assignment insn)))
        (else
         (let ((match-spec (sasm-match-insn (rewrite-simple-result-assignment insn))))
           (and match-spec
                (matchrec (car match-spec) 
                          (list-ref match-spec 1) 
                          (list-ref match-spec 3)
                          (list-ref match-spec 4)
                          '() 
                          live-in 
                          live-out))))))
