;; csharp-lex.scm
;;

(need regex/regex)
(need regex/genlex)
(need util/list)

;; Some basic definitions to aid in building regular expressions

(define (chars string)
  string)

(define (chars-append . args)
  (apply string-append args))

(define (chars-without chars exclude)
  (list->string (filter (lambda (char) (not (strchr exclude char)))
                        (string->list chars))))

(define alpha (chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))
(define numeric (chars "0123456789"))
(define whitespace (chars "\r\t\n "))
(define special (chars "~!@#%^&*()-=+{[]}|:;'<>,.?/_\""))
(define ident-start (chars-append alpha (chars "_")))
(define ident (chars-append ident-start numeric))
(define string-chars (chars-append alpha numeric (chars-without special "\"")
                                   (chars " ")))
(define slashslash-comment-chars (chars-append alpha numeric special (chars-without whitespace "\n")))
(define slashstar-comment-chars (chars-append alpha numeric (chars-without special "*/") whitespace))
(define integer-pattern `(& (+ (@ ,numeric)) (^ (: #\u #\U #\l #\L))))
(define identifier-pattern `(& (@ ,ident-start) (* (@ ,ident))))
(define string-pattern `(& #\" 
                           (* (: (& #\\ (@ "0nrt\"\\"))
                                 (@ ,string-chars)))
                           #\"))
(define char-pattern `(& #\'
                         (: (@ ,(chars-append alpha numeric (chars " ")
                                              (chars-without special "'")))
                            (& #\\ (@ "0nrt\\'")))
                         #\'))
(define slashstar-comment-pattern `(& #\/ #\* 
                                      (* (@ ,(chars-append slashstar-comment-chars (chars "/"))))
                                      #\*
                                      (* #\*)
                                      (* (& (@ ,slashstar-comment-chars)
                                            (* ,(chars-append slashstar-comment-chars (chars "/")))
                                            #\*
                                            (* #\*)))
                                      #\/))

(define tokens
  `((*whitespace* (* (@ ,whitespace)))
    (*comment* (& #\/ #\/ (* (@ ,slashslash-comment-chars)) #\newline))
    (*comment* ,slashstar-comment-pattern)
    (--sasm-impl "__sasm_impl")
    (class "class")
    (super "super")
    (length "length")
    (new "new")
    (l-paren "(")
    (r-paren ")")
    (l-curl "{")
    (r-curl "}")
    (public "public")
    (static "static")
    (void "void")
    (l-brak "[")
    (r-brak "]")
    (colon ":")
    (s-colon ";")
    (int "int")
    (string "string")
    (char "char")
    (boolean "bool")
    (return "return")
    (if "if")
    (while "while")
    (else "else")
    (virtual "virtual")
    (override "override")
    (= "=")
    (&& "&&")
    (< "<")
    (== "==")
    (> ">")
    (+ "+")
    (- "-")
    (* "*")
    (dot ".")
    (? "?")
    (>>> ">>>")
    (>> ">>")
    (<< "<<")
    (pipe "|")
    (& "&")
    (~ "~")
    (^ "^")
    (true "true")
    (this "this")
    (false "false")
    (system-console-writeline "System.Console.WriteLine")
    (fail "fail")
    (bang "!")
    (comma ",")
    (char-literal ,char-pattern)
    (string-literal ,string-pattern)
    (ident ,identifier-pattern)
    (number ,integer-pattern)
    ))

(define csharp-lexer (generate-lexer tokens
                                       '(*whitespace* *comment*)))

(define lexer-filename "csharp-scanner.scm")

(define (main)
  (if (file-exists? lexer-filename)
      (delete-file lexer-filename))

  (call-with-output-file lexer-filename
    (lambda (port)
      (write-lexer csharp-lexer 
                   'csharp-lexer-dfa
                   'csharp-lexically-analyze
                   port))))

(main)
