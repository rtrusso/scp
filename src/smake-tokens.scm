;; smake-tokens.scm
;;
;; This module uses the lexer generator to create a lexer for the
;; smake tool.
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

;; The bulk of a regular expression is defining the allowable
;; characters.  The following definitions of character sets will allow
;; me to compose them into other character sets

;; alpha -> (a-z|A-Z)
(define alpha (chars "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"))

;; numeric -> (0-9)
(define numeric (chars "0123456789"))

;; whitespace -> space | carriage-return | newline | tab
(define whitespace (chars "\r\t\n "))

;; special characters
(define special (chars "`~!@#$%^&*()-=+{[]}|:;'<>,.?/_\""))

;; The start character of an identifier can be an alphabet character
;; or an underscore
(define ident-start (chars-append alpha (chars "_")))

;; An internal identifier character can be any start character, plus
;; any numeric character.
(define ident (chars-append ident-start numeric))

;; These are the characters that are allowed to fall in between the
;; quotes of a string literal, *discounting* escape sequences.  Escape
;; sequences and the surrounding quotes will be handled in the RE
;; definition below.
;;
(define string-chars (chars-append alpha numeric (chars-without special "\"")
                                   (chars " ")))

;; Characters allowed to fall inside of a "//" comment.
(define slashslash-comment-chars (chars-append alpha numeric special (chars-without whitespace "\n")))
(define slashstar-comment-chars (chars-append alpha numeric (chars-without special "*/") whitespace))

;; Most of the tokens in our lexer are simple keywords.  There are
;; only a few that are handled by "real" RE's.  I'm defining these
;; below.
;;
;; My RE language is very similar (and I'll argue *equivalent*) to the
;; standard RE syntax taught in academic settings.  However, since
;; this is Scheme and s-exp's are the most convenient representation
;; mechanism, I'm translating them into a prefix syntax.
;;
;; Standard Academic Syntax             | Scheme                | Notes
;; -------------------------------------+-----------------------+----------------
;; a*                                   | (* a)                 |
;; a+                                   | (+ a)                 |
;; (a | b)                              | (: a b)               |
;; (a | the-empty-string)               | (^ a)                 | "a is optional"
;; abc                                  | (& a b c)             | "sequencing operator"
;; abc                                  | "abc"                 | "sequencing characters only"
;; 0-9                                  | (@ "0123456789")      | ***
;;
;;
;; *** (@ "abc123@$") is shorthand for (a|b|c|1|2|3|@||$).  Since this
;; is a common case and would add lots of states to the NFA for the
;; regex, expressions using this syntax are also carefully optimized.
;;

;; == integers ==
;;
;;   A special note on integer constants: the leading '+' or '-' character
;;   is not handled in the lexical analysis phase, but later in the parsing
;;   phase.  So the '+/-' is not included in this regular expression.
;;
;;   Standard:
;;     (0-9)+(u|U|l|L|the-empty-string)
;;
;;   S-Exp:
;;     (& (+ (@ "0123456789")) (^ (: u U l L)))
;;
(define integer-pattern `(& (+ (@ ,numeric)) (^ (: #\u #\U #\l #\L))))

;; == identifiers ==
;;
;;   Standard:
;;     (a-z|A-Z|_)(a-z|A-Z|0-9|_)*
;;
;;   S-Exp:
;;     (& (@ "a...zA...Z_") (* (@ "a..zA..Z_0..9")))
;;    
(define identifier-pattern `(& (@ ,ident-start) (* (@ ,ident))))

;; == freeform-identifier ==
;;
;;   Standard:
;;     (0-9|a-z|A-Z|_|.|/|%|*|?|-)
;;
;;   S-Exp:
;;     (& (* (@ "a..zA..Z_./%*?")))
(define freeform-identifier-special-chars (chars "./%*?-~"))
(define freeform-identifier-chars
  (chars-append ident
                freeform-identifier-special-chars))
(define freeform-identifier-pattern `(& (* (@ ,freeform-identifier-chars))))

;; == string literals ==
;;
;;   Standard:
;;     "(a-z|A-Z|0-9|!|@|$|...|\(n|r|t|"|\))*"
;;
;;   S-Exp:
;;     (& #\" (* (: (& #\\ (@ "nrt\"\\")) (@ "a..zA..Z0..9!..."))) ")
;;
;;  NOTE: Syntax for a character literal in Scheme is #\c.
;;
;;  C/C++    Scheme
;;  'a'      #\a
;;  '#'      #\#
;;  '\\'     #\\
;;  '\''     #\'
;;  '"'      #\"
;;  ' '      #\space
;;  '\n'     #\newline
;;
(define string-pattern `(& #\" 
                           (* (: (& #\\ (@ "0nrt\"\\"))
                                 (@ ,string-chars)))
                           #\"))

;; == char literal ==
;;
;; 'a' 'b' 'c' '\'' '"' '\\'
;;
(define char-pattern `(& #\'
                         (: (@ ,(chars-append alpha numeric (chars " ")
                                              (chars-without special "'")))
                            (& #\\ (@ "0nrt\\'")))
                         #\'))

(define tokens
  `((tab #\tab)
    (newline (: "\r\n" "\n"))
    (whitespace (& #\space (* (@ " \t"))))
    (comment (& #\# (* (@ ,slashslash-comment-chars)) #\newline))
    (suppress-newline "\\\n")
    (ifeq "ifeq")
    (else "else")
    (endif "endif")
    (backslash #\\)
    (dollar #\$)
    (lparen #\()
    (rparen #\))
    (percent #\%)
    (dot #\.)
    (colon #\:)
    (equals #\=)
    (comma #\,)
    (less-than #\<)
    (greater-than #\>)
    (at #\@)
    (char-literal ,char-pattern)
    (string-literal ,string-pattern)
    (number ,integer-pattern)
    (identifier ,identifier-pattern)
    (freeform-identifier ,freeform-identifier-pattern)
    ))

(define smake-lexer (generate-lexer tokens
                                    '(*whitespace* *comment*)))

(define lexer-filename "smake-lexer.scm")

(define (main)
  (if (file-exists? lexer-filename)
      (delete-file lexer-filename))

  (call-with-output-file lexer-filename
    (lambda (port)
      (write-lexer smake-lexer 
                   'smake-lexer-dfa
                   'smake-lexically-analyze
                   port))))

(main)

