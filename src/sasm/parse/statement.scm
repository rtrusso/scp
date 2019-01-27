(need sasm/parse/syntax)
(need sasm/sasm-ast)
(need sasm/sasm-tracing)
(need sasm/parse/extern)

(define (sasm-parse-statement statement)
  (sasm-parse-by-case
   statement

   (sasm-syntax-case
    :pattern (class (,symbol? class-symbol)
                    (const (,integer? class-data-size))
                    .
                    (@@ ,sasm-parse-class-member-function
                        member-functions))
    :rewrite (class-symbol class-data-size member-functions)
    (sasm-ast-node <sasm-class>
                   (:defined-symbol class-symbol)
                   (:qualified-symbol #f)
                   (:resolved-references '())
                   (:size class-data-size)
                   (:member-functions member-functions)))

   (sasm-syntax-case
    :pattern (global (,symbol? global-symbol)
                     .
                     (@@ ,sasm-parse-global-data-entry
                         global-data-entries))
    :rewrite (global-symbol global-data-entries)
    (sasm-ast-node <sasm-global-data>
                   (:defined-symbol global-symbol)
                   (:qualified-symbol #f)
                   (:resolved-references '())
                   (:global-data-symbol global-symbol)
                   (:global-data global-data-entries)))

   (sasm-syntax-case
    :pattern (entry (,symbol? entry-symbol))
    :rewrite (entry-symbol)
    (sasm-ast-node <sasm-entry-point>
                   (:referenced-symbol entry-symbol)
                   (:resolved-symbol #f)
                   (:entry-point-symbol entry-symbol)))

   (sasm-syntax-case
    :pattern (define-symconst . (@@ ,sasm-parse-symconst-entry
                                    symconst-entries))
    :rewrite (symconst-entries)
    (sasm-ast-node <sasm-symconst-table>
                   (:symconst-entries symconst-entries)))

   (sasm-syntax-case
    :pattern (class-info . (? class-info-body))
    :rewrite (class-info-body)
    (sasm-ast-node <sasm-class-info>
                   (:class-info-body class-info-body)))

   (sasm-syntax-case
    :pattern (extern (,symbol? extern-symbol))
    :rewrite (extern-symbol)
    (cons-sasm-extern extern-symbol))

   (sasm-syntax-case
    :pattern (export (,symbol? export-symbol))
    :rewrite (export-symbol)
    (sasm-ast-node <sasm-export>
                   (:referenced-symbol export-symbol)
                   (:resolved-symbol #f)))

   (sasm-syntax-case
    :pattern (include (,string? include-path))
    :rewrite (include-path)
    (sasm-ast-node <sasm-include>
                   (:include-path include-path)))

   (sasm-syntax-case
    :pattern (function (name (,symbol? function-symbol))
                       (locals (,integer? local-count))
                       (body . (@@ ,sasm-parse-instruction
                                   instructions)))
    :rewrite (function-symbol local-count instructions)
    (sasm-ast-node <sasm-function>
                   (:defined-symbol function-symbol)
                   (:qualified-symbol #f)
                   (:resolved-references '())
                   (:function-symbol function-symbol)
                   (:local-count local-count)
                   (:instructions instructions)))

   (sasm-parse-error
    :error-message "Invalid statement:")

   ))
