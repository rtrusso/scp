;; machdesc.scm
;; machine description syntax and functions

(define (machine-description . args)
  args)

;; instruction syntax ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax instruction
  (syntax-rules (input-pattern rewrite-rule transform insel-rewrite-rule)

    ((instruction (input-pattern <pattern>)
                  (rewrite-rule (<format-string> (<match-symbol> <expression>) ...) ...))
     (instruction (input-pattern <pattern>)
                  (rewrite-rule (<format-string> (<match-symbol> <expression>) ...) ...)
                  (side-effects)))

    ((instruction (input-pattern <pattern>)
                  (rewrite-rule <format-string> (<match-symbol> <expression>) ...))
     (instruction (input-pattern <pattern>)
                  (rewrite-rule (<format-string> (<match-symbol> <expression>) ...))))
    
    ((instruction (input-pattern <pattern>)
                  (rewrite-rule (<format-string> (<match-symbol> <expression>) ...) ...)
                  (side-effects <side-effect> ...))
     (list <pattern>
           (list (list (quasiquote <format-string>) (cons '<match-symbol> <expression>) ...) ...)
           (list '<side-effect> ...)
           #f))

    ((instruction (input-pattern <pattern>)
                  (rewrite-rule (<format-string> (<match-symbol> <expression>) ...) ...)
                  (side-effects <side-effect> ...)
                  (insel-rewrite-rule <sasm-exp> ...))
     (list <pattern>
           (list (list (quasiquote <format-string>) (cons '<match-symbol> <expression>) ...) ...)
           (list '<side-effect> ...)
           (list (quasiquote <sasm-exp>) ...)))

    ))

