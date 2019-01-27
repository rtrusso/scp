(need parse/genparse)

;; Note: By convention, nonterminals are prefixed with '@' and
;; terminals do not have a prefix.

(define smake-default-rule (lambda args args))

(define smake-grammar-rules
  '((@goal @makefile)
    (:rule (lambda args args))
    (@makefile @statement-list)
    (:rule (lambda args args))
    (@statement-list @statement)
    (:rule (lambda args args))
    (@statement-list @statement-list @statement)
    (:rule (lambda args args))
    (@statement @definition)
    (:rule (lambda args args))
    (@statement @rule)
    (:rule (lambda args args))
    (@statement @empty-line)
    (:rule (lambda args args))

    (@definition identifier equals @expression newline)
    (:rule (lambda args args))

    (@rule @left-hand-side colon @element-list newline @rule-body)
    (:rule (lambda args args))

    (@rule-body @rule-instruction)
    (:rule (lambda args args))
    (@rule-body @rule-body @rule-instruction)
    (:rule (lambda args args))

    (@rule-instruction tab @expression newline)
    (:rule (lambda args args))

    (@left-hand-side @complex-identifier)
    (:rule (lambda args args))

    (@empty-line newline)
    (:rule (lambda args args))

    ;;
    ;; complex-identifier
    ;;

    (@complex-identifier @simple-identifier-list)
    (:rule (lambda args args))

    ;;
    ;; simple-identifier-list
    ;;
    (@simple-identifier-list @simple-identifier)
    (:rule (lambda args args))
    (@simple-identifier-list @simple-identifier-list @simple-identifier)
    (:rule (lambda args args))

    ;;
    ;; simple-identifier
    ;;
    (@simple-identifier identifier)
    (:rule (lambda args args))
    (@simple-identifier freeform-identifier)
    (:rule (lambda args args))
    (@simple-identifier @symbol-reference)
    (:rule (lambda args args))
    (@simple-identifier whitespace)
    (:rule (lambda args args))

    ;;
    ;; expression
    ;;
    (@expression @element-list)
    (:rule (lambda args args))

    ;;
    ;; element-list
    ;;
    (@element-list @element)
    (:rule (lambda args args))
    (@element-list @element-list @element)
    (:rule (lambda args args))

    ;;
    ;; element
    ;;
    (@element @input-symbol)
    (:rule (lambda args args))
    (@element @output-symbol)
    (:rule (lambda args args))
    (@input-symbol dollar less-than)
    (:rule (lambda args args))
    (@output-symbol dollar at)
    (:rule (lambda args args))
    (@element identifier)
    (:rule (lambda args args))
    (@element @special-element)
    (:rule (lambda args args))

    (@special-element @symbol-reference)
    (:rule (lambda args args))
    (@special-element @function-invocation)
    (:rule (lambda args args))

    (@symbol-reference dollar lparen identifier rparen)
    (:rule (lambda args args))

    (@function-invocation dollar lparen identifier whitespace @expression rparen)
    (:rule (lambda args args))

    (@element freeform-identifier)
    (:rule (lambda args args))
    (@element whitespace)
    (:rule (lambda args args))
    (@element tab)
    (:rule (lambda args args))
    (@element percent)
    (:rule (lambda args args))
    (@element dot)
    (:rule (lambda args args))
    (@element colon)
    (:rule (lambda args args))
    (@element equals)
    (:rule (lambda args args))
    (@element comma)
    (:rule (lambda args args))
    (@element less-than)
    (:rule (lambda args args))
    (@element greater-than)
    (:rule (lambda args args))
    (@element at)
    (:rule (lambda args args))
    ))

(define smake-grammar 
  (grammar-rules-filter-productions smake-grammar-rules))

(define smake-graph (build-lr0-state-transition-graph smake-grammar))
(define smake-sr-table (generate-lr0-table smake-grammar smake-graph smake-grammar-rules))
(define smake-goto-table (generate-lr0-goto-table smake-grammar smake-graph))
(define smake-terminals (grammar-terminals smake-grammar))
(define smake-nonterminals (grammar-nonterminals smake-grammar))
(define smake-action-table (grammar-rules-build-action-table smake-grammar-rules))

(if (file-exists? "smake-parser.scm")
    (delete-file "smake-parser.scm"))

(call-with-output-file "smake-parser.scm"
  (lambda (port)
    (for-each
     (lambda (code) (write code port) (newline port))
     `((need parse/parse)
       (define action-table ,smake-action-table)
       (define (smake-parse-input input)
         (run-parse (quote ,smake-grammar)
                    (quote ,(append smake-terminals (list '$)))
                    (quote ,smake-nonterminals)
                    (quote ,smake-sr-table)
                    (quote ,smake-goto-table)
                    action-table
                    input))
       (define (smake-parse-port input-port)
         (smake-parse-input (read input-port)))
       (define (smake-parse-file filename)
         (call-with-input-file filename
           smake-parse-port))))))
