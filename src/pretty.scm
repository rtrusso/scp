(define (pretty-print-ast type node indent)
  (define (print indent? . args)
    (if indent?
        (let loop ((i 0))
          (if (< i indent)
              (begin (display " ") (loop (+ i 1))))))
    (if (not (list? args))
        (error "Unable to print args - not a proper list"))
    (for-each (lambda (x) (cond ((symbol? x) 
                                 (display (ast-node-attr node x)))
                                ((and (list? x) (= 2 (length x)) (eqv? '@ (car x)))
                                 (accept-visitor (ast-node-attr node (cadr x)) pretty-print-ast 0))
                                (else (display x))))
              args))
  (define (println indent? . args)
    (apply print indent? args)
    (newline)
    (display ";"))
  (define (: attr)
    (ast-node-attr node attr))
  (define (print-arglist list)
    (print #f "(")
    (if (not (null? list))
        (begin (print #f " ")
               (accept-visitor (car list) pretty-print-ast 0)
               (if (not (list? list))
                   (error "Unable to print arglist - not a proper list"))
               (for-each (lambda (child)
                           (print #f ", ")
                           (accept-visitor child pretty-print-ast 0))
                         (cdr list))
               (print #f " ")))
    (print #f ")"))
  (case type
    ((program)
     (display ";")
     (visit-each pretty-print-ast
                 (: ':class-list)
                 0)
     (newline))
    ((class)
     (print #t "class " ':class-name)
     (if (: ':parent) 
         (println #f " extends " ':parent)
         (println #f))
     (println #t "{")
     (visit-each pretty-print-ast
                 (: ':instance-vars)
                 (+ indent 2))
     (if (not (null? (: ':instance-vars)))
         (println #f))
     (visit-each pretty-print-ast
                 (: ':static-vars)
                 (+ indent 2))
     (if (not (null? (: ':static-vars)))
         (println #f))
     (visit-each pretty-print-ast
                 (: ':constructors)
                 (+ indent 2))
     (visit-each pretty-print-ast
                 (: ':methods)
                 (+ indent 2))
     (visit-each pretty-print-ast
                 (: ':static-methods)
                 (+ indent 2))
     (println #t "}")
     (println #f))

    ((var)
     (println #t '(@ :type) " " ':name ";"))
    ((static-var)
     (println #t "static " '(@ :type) " " ':name ";"))
    ((method)
     (print #t "public " 
            (if (: ':static?) "static " "")
            '(@ :return-type) " " ':name "")
     (print-arglist (: ':param-list))
     (println #f "")
     (println #t "{")
     (visit-each pretty-print-ast
                 (: ':local-vars)
                 (+ indent 2))
     (if (not (null? (: ':local-vars)))
         (println #f ""))
     (visit-each pretty-print-ast
                 (: ':statements)
                 (+ indent 2))
     (println #t "}")
     (println #f))

    ((constructor)
     (print #t "public " '(@ :return-type))
     (print-arglist (: ':param-list))
     (println #f "")
     (println #t "{")
     (visit-each pretty-print-ast
                 (: ':local-vars)
                 (+ indent 2))
     (if (not (null? (: ':local-vars)))
         (println #f ""))
     (visit-each pretty-print-ast
                 (: ':statements)
                 (+ indent 2))
     (println #t "}")
     (println #f))

    ((compound-statement)
     (println #t "{")
     (visit-each pretty-print-ast
                 (: ':statements)
                 (+ indent 2))
     (println #t "}"))

    ((return-statement)
     (println #t "return " '(@ :exp) ";"))

    ((if-statement)
     (println #t "if( " '(@ :condition) " )")
     (accept-visitor (: ':consequent) pretty-print-ast (+ indent 2))
     (println #t "else")
     (accept-visitor (: ':antecedent) pretty-print-ast (+ indent 2)))

    ((while-statement)
     (println #t "while( " '(@ :condition) " )")
     (accept-visitor (: ':body) pretty-print-ast (+ indent 2)))

    ((print-statement)
     (println #t "System.out.println( " '(@ :exp) " );"))

    ((assignment-statement)
     (print #t ':ident " = ")
     (accept-visitor (: ':exp) pretty-print-ast 0)
     (println #f ";"))

    ((array-assignment-statement)
     (print #t ':ident "[ ")
     (accept-visitor (: ':index-exp) pretty-print-ast 0)
     (print #f " ] = ")
     (accept-visitor (: ':value-exp) pretty-print-ast 0)
     (println #f ";"))

    ((simple-type)
     (print #f ':name))

    ((array-type)
     (print #f '(@ :element-type) "[]"))

    ((user-type)
     (print #f ':name))

    ((param)
     (print #f '(@ :type) " " ':name))

    ((and-expression less-than-expression add-expression subtract-expression multiply-expression
                     equal-to-expression)
     (print #t 
            "( "
            '(@ :left)
            " "
            (case type 
              ((and-expression) "&&") 
              ((less-than-expression) "<")
              ((add-expression) "+")
              ((subtract-expression) "-")
              ((multiply-expression) "*")
              ((equal-to-expression) "==")
              (else (error "Invalid binary expression type " type)))
            " "
            '(@ :right)
            " )"))

    ((not-expression)
     (print #f "!( " '(@ :exp) " )"))

    ((array-ref-expression)
     (print #f '(@ :array-exp) "[ " '(@ :index-exp) " ]"))

    ((length-expression)
     (print #f '(@ :exp) ".length"))

    ((method-call-expression)
     (print #f '(@ :object) "." ':method-name "")
     (print-arglist (: ':arg-list))
     (print #f (if (tailcall? node) " /* tailcall */" "")))

    ((boolean-constant-expression number-constant-expression char-constant-expression string-constant-expression)
     (print #f ':value))

    ((variable-expression)
     (print #f ':ident))

    ((this-expression)
     (print #f "this"))

    ((new-array-expression)
     (print #f "new " '(@ :element-type) "[ " '(@ :length) " ]"))

    ((new-object-expression)
     (print #f "new " ':class-name "")
     (print-arglist (: ':arg-list)))

    (else
     (error "Pretty-Print: Unknown AST node type " type node))))
