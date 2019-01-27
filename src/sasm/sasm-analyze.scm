;; sasm-analyze.scm
;;
;; A component of the SASM interpreter which analyzes SASM source,
;; performing static syntactic and semantic analysis which validates
;; the code and prepares it for efficient runtime interpretation.

(need sasm/sasm-ast)
(need sasm/sasm-visitor)

;;
;; == SASM ANALYSIS DESIGN NOTES ==
;;
;; ==== Modules
;;
;; The SASM analyzer is capable of loading multiple SASM source
;; modules into the same analysis session in a way that simulates the
;; process of linking multiple modules when the SASM code is
;; translated to object code.
;;
;; The scoping of globally imported and exported symbols, as well as
;; module-local symbols, is of key importantance to this simulation.
;;
;; ==== Scopes
;;
;;  - Program Scope; a Program contains multiple Modules.  Exported
;;  and imported global variables exist in Program Scope.
;;
;;  - Module Scope; A Module contains multiple Functions.  Symbols
;;  that are declared in the module outside of the body of a function
;;  are module-local unless exported.
;;
;;  - Function Scope; A Function contains multiple instructions which
;;  may reference labels in any of the scopes.  Labels defined in a
;;  function are local to the Function.
;;

(define (sasm-program-extern-symbols program)
  (let ((result '()))
    (define-sasm-ast-visitor visitor
      (define-case ((extern <sasm-extern>)
                    (symbol :extern-symbol))
        (set! result (cons symbol result))))
    (sasm-visit-ast program visitor)
    result))

;; Retrieves all of the symbols defined by the specified SASM program
;; abstract syntax tree.  These may or may not be exported.  Imported
;; symbols are not "defined".  This returns top-level symbols only.
;;
(define (sasm-program-defined-symbols program)
  (let ((result '()))
    (define (accumulate-symbol symbol)
      (if (member symbol result)
          (error "Multiple definitions of symbol" symbol)
          (set! result (cons symbol result))))

    (define-sasm-ast-visitor visitor
      (define-case ((class <sasm-class>)
                    (symbol :defined-symbol))
        (accumulate-symbol symbol))

      (define-case ((global <sasm-global-data>)
                    (symbol :defined-symbol))
        (accumulate-symbol symbol))

      (define-case ((function <sasm-function>)
                    (symbol :defined-symbol))
        (accumulate-symbol symbol)))

    (sasm-visit-ast program visitor)
    result))

;; Returns a list of all unique symbols referenced in the specified
;; (and perhaps partial) abstract syntax tree.
;;
(define (sasm-program-referenced-symbols ast)
  (define result '())
  (define (accumulate-symbol! symbol)
    (if (not (member symbol result))
        (set! result (cons symbol result))))
  (define-sasm-ast-visitor visitor
    (define-case ((operand <label-constant-operand>)
                  (symbol :label-value))
      (accumulate-symbol! symbol)))
  (sasm-visit-ast ast visitor)
  (reverse result))

(define (sasm-function-defined-labels function)
  (let ((result
         (sasm-visit-ast-with-accumulator
          accumulate!
          function

          (define-case ((label-object <sasm-label-definition-directive>)
                        (label-symbol :label-name))
            (accumulate! label)))))
    (let* ((result-symbols (map (lambda (x) (sasm-ast-node-attribute x :label-name))
                                result)))
      (let ((no-dupes (remove-duplicates eqv? result-symbols)))
        (if (not (equal? no-dupes result-symbols))
            (error "Duplicate labels defined in function "
                   (subtract-lists eqv? result-symbols no-dupes)))
        result))))

(define (sasm-program-analyze-symbols! module-prefix program)
  (define status #t)
  (define current-scope '())
  (define scope-depth 0)
  (define current-program #f)
  (define current-function #f)
  (define undefined-symbols '())

  (define (symbol-error message . args)
    (display "ERROR: ")
    (display message)
    (for-each display args)
    (newline)
    (set! status #f))

  (define (look-up-symbol-in-scope symbol scope)
    (and (not (null? scope))
         (let ((entry (assoc symbol (cdr scope))))
           (or entry
               (look-up-symbol-in-scope symbol (car scope))))))

  (define (look-up-symbol symbol)
    (look-up-symbol-in-scope symbol current-scope))

  (define (prefix-symbol prefix symbol . symbols)
    (string->symbol (apply string-append (map (lambda (x)
                                                (if (string? x)
                                                    x
                                                    (symbol->string x)))
                                              (cons prefix (cons symbol symbols))))))

  (define (push-scope! symbols)
    (set! scope-depth (+ 1 scope-depth))
    (set! current-scope (cons current-scope symbols)))

  (define (pop-scope!)
    (set! scope-depth (+ 1 scope-depth))
    (set! current-scope (car current-scope)))

  (define (check-duplicates context symbol-list)
    (define (iter list)
      (cond ((null? list) #t)
            ((member (car symbol-list) (cdr symbol-list))
             (symbol-error "Duplicate symbol detected " context " " (car symbol-list)))
            (else
             (iter (cdr list)))))

    (iter symbol-list))

  (define (resolve-reference! node)
    (let* ((symbol (sasm-ast-node-attribute node :referenced-symbol))
           (value (look-up-symbol symbol)))
      (if (not value)
          (if (not (member symbol undefined-symbols))
              (begin (symbol-error "Undefined symbol [" symbol "]")
                     (set! undefined-symbols (cons symbol undefined-symbols))))
          (begin
            (sasm-ast-node-attribute! node :resolved-symbol
                                      (cdr value))
            (sasm-ast-node-append-attribute! (cdr value) :resolved-references
                                             node)))))

  (define-sasm-ast-visitor visitor

    ;;
    ;; Push a top-level scope when processing the main program node.
    ;;
    (define-case ((program <sasm-program>))
      (:preorder
       (let* ((externs (sasm-filter-ast program <sasm-extern>))
              (defined (sasm-filter-ast program
                                        <sasm-class>
                                        <sasm-global-data>
                                        <sasm-function>))
              (defined-symbols  (map (sasm-ast-getter :defined-symbol)
                                     defined))
              (extern-symbols (map (sasm-ast-getter :defined-symbol)
                                   externs))
              (top-level (append externs defined))
              (top-level-symbols (map (sasm-ast-getter :defined-symbol)
                                      top-level))
              (exports (sasm-filter-ast program <sasm-export>))
              (export-symbols (map (sasm-ast-getter :referenced-symbol) exports))
              )
         (check-duplicates "" top-level-symbols)
         (for-each (lambda (export)
                     (if (not (member export defined-symbols))
                         (symbol-error "Symbol exported but not defined [" export "]")))
                   export-symbols)
         (for-each (lambda (node symbol)
                     (if (and (not (member symbol export-symbols))
                              (not (member symbol extern-symbols)))
                         (sasm-ast-node-attribute! node :qualified-symbol
                                                   (prefix-symbol "@"
                                                                  module-prefix
                                                                  ":"
                                                                  symbol))
                         (sasm-ast-node-attribute! node :qualified-symbol symbol)))
                   top-level
                   top-level-symbols)
         (set! current-program program)
         (push-scope! (map cons top-level-symbols top-level))))
      (:postorder
       (set! current-program #f)
       (pop-scope!)))

    ;;
    ;; Push a function-level scope when processing each function node.
    ;;
    (define-case ((function <sasm-function>)
                  (function-symbol :defined-symbol))
      (:preorder
       (let* ((label-directives (sasm-filter-ast function <sasm-label-definition-directive>))
              (label-symbols (map (sasm-ast-getter :defined-symbol) label-directives)))
         (for-each (lambda (node symbol)
                     (sasm-ast-node-attribute! node :qualified-symbol
                                               (prefix-symbol "@"
                                                              module-prefix
                                                              ":"
                                                              function-symbol
                                                              ":"
                                                              symbol)))
                   label-directives
                   label-symbols)
         (check-duplicates function-symbol label-symbols)
         (push-scope! (map cons label-symbols label-directives))
         (set! current-function function))
       )
      (:postorder
       (set! current-function #f)
       (pop-scope!)))

    (define-case ((reference <label-constant-operand>
                             <sasm-member-function>
                             <sasm-global-data-symbol>
                             <sasm-entry-point>
                             <sasm-export>))
      (resolve-reference! reference))

    )

  (sasm-visit-ast program visitor)
  status)
