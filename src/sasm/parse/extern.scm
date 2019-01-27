(need sasm/sasm-tracing)
(need sasm/sasm-ast)

;; (define-syntax define-sasm-ast-schema2
;;   (syntax-rules (node-definition)
;;     ((_ <schema-checker>
;;         (node-definition <type> <attribute-1> <attribute-2> ...) ...)
;;      (define <schema-checker>
;;        (let* ((known-types '(<type> ...))
;;               (unused-1 (if *sasm-enable-debug-output*
;;                             (begin (display ";; ast-schema2 known-types ")
;;                                    (display known-types))))
;;               (no-dupes (remove-duplicates eqv? known-types)))
;;          (if (not (equal? known-types no-dupes))
;;              (error "Duplicate SASM AST node types defined!"))
;;          (lambda (node)
;;            (if *sasm-enable-debug-output*
;;                (begin (display ";; ast-schema2 invoke ")
;;                       (display known-types)
;;                       (newline)
;;                       (display ";; node ")
;;                       (display node)
;;                       (newline)
;;                       ))
;;            (if *sasm-enable-debug-output*
;;                (begin (display ";; get-tag of node ")
;;                       (display (get-tag node))
;;                       (newline)))
;;            (case (get-tag node)
;;              ((<type>)
;;               (let ((known-attributes '(<attribute-1> <attribute-2> ...))
;;                     (actual-attributes (map car (contents node))))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; known-attributes ")
;;                            (display known-attributes)
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; actual-attributes ")
;;                            (display actual-attributes)
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; subset-1 ")
;;                            (display (list-subset? eqv? known-attributes actual-attributes))
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; subset-2 ")
;;                            (display (list-subset? eqv? actual-attributes known-attributes))
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; len ")
;;                            (display (length actual-attributes))
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; remove-dupes-1 ")
;;                            (display (remove-duplicates eqv? known-attributes))
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; remove-dupes-2 ")
;;                            (display (remove-duplicates eqv? actual-attributes))
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; len-remove-dupes-1 ")
;;                            (display (length (remove-duplicates eqv? known-attributes)))
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; len-remove-dupes-2 ")
;;                            (display (length (remove-duplicates eqv? actual-attributes)))
;;                            (newline)))
;;                 (if *sasm-enable-debug-output*
;;                     (begin (display ";; clause ")
;;                            (display (= (length actual-attributes)
;;                                        (length (remove-duplicates eqv? known-attributes))
;;                                        (length (remove-duplicates eqv? actual-attributes))))
;;                            (newline)))
;;                 (or (and (list-subset? eqv? known-attributes actual-attributes)
;;                          (list-subset? eqv? actual-attributes known-attributes)
;;                          (= (length actual-attributes)
;;                             (length (remove-duplicates eqv? known-attributes))
;;                             (length (remove-duplicates eqv? actual-attributes))))
;;                     (error "SASM AST schema violation "
;;                            (get-tag node)
;;                            known-attributes
;;                            actual-attributes))))
;;              ...
;;              (else
;;               (error "Unknown AST node type" (get-tag node))))))))))

;; (define-sasm-ast-schema2 sasm-ast-schema2

;;   (node-definition <sasm-extern>
;;                    :defined-symbol
;;                    :qualified-symbol
;;                    :resolved-references
;;                    :extern-symbol)

;;   )


;; (define-syntax sasm-ast-node2
;;   (syntax-rules ()
;;     ((_ <type> (:name value) ...)
;;      (let ((attribs (list (cons ':name value) ...)))
;;        (if *sasm-enable-debug-output*
;;            (begin (display ";;   attribs ")
;;                   (display attribs)
;;                   (newline)))
;;        (let ((node (tag '<type> attribs)))
;;          (if *sasm-enable-debug-output*
;;              (begin (display "   node2 ")
;;                     (display node)
;;                     (newline)
;;                     (sasm-ast-schema2 node)
;;                     (display "   after sasm-ast-schema")
;;                     (newline)))
;;          node)))))

(define (cons-sasm-extern extern-symbol)
  (debug sasm-parse-extern-symbol extern-symbol)
  (let ((ast-node (sasm-ast-node <sasm-extern>
                                 (:defined-symbol extern-symbol)
                                 (:qualified-symbol #f)
                                 (:resolved-references '())
                                 (:extern-symbol extern-symbol))))
    (debug sasm-parse-extern-symbol-ast " " ast-node)
    ast-node))
