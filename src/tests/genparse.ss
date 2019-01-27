(need parse/genparse)

(define grammar1
  '((a b c)
    (a x)
    (b comma c comma)
    (c l-paren a r-paren)))

(define grammar
  '((exp foo)
    (foo bar)
    (foo foostart bar)
    (foostart foo comma)))

(define s (grammar-start-production grammar))
(define i (production-initial-item s))
(define c (state-from-item-closure grammar i))
(define t (build-lr0-state-transition-graph grammar))
(print-state-transition-graph t)
