;; generate-java-lexer.scm
;;
;; adapted from minijava-tokens.scm
;;
;;

(need regex/regex)
(need regex/genlex)
(need util/list)
(need minijava-lexer-java)

(define lexer-filename "scp/frontend/java/lexer/JavaLexerData.java")

(define (main)
  (if (file-exists? lexer-filename)
      (delete-file lexer-filename))

  (call-with-output-file lexer-filename
    (lambda (port)
      (write-java-lexer-from-dfa minijava-lexer-dfa
                                 "scp.frontend.java.lexer"
                                 #f
                                 "final"
                                 "JavaLexerData"
                                 port))))

(main)
