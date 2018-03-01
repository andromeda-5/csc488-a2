#lang racket #| Compile Language L0 to Language L1 |#

(provide debruijn index-λs L0→L1)
(module+ test (require rackunit))
(require racket/syntax)

#| Language L0 is a slightly enriched version of the LC.

 An L0 expression is defined structurally [recall CSC236 which covered reading such definitions].
 It's one of: |#
#;(L0: λ (<id>) <e>)
#;(L0: app <e1> <e2>)
#;(L0: var <id>)
#;(L0: datum <i>)
#;(L0: set! <id> <e>)
;  where <e>, <e1>, <e2> are L0 expressions, <id> is an identifier, and <i> is an integer.

; As shown, in this assignment we'll tag language terms with an initial symbol indicating which
;  language they're from.

; As usual, our runtime representation of such expressions is racket lists for parenthesized terms,
;  and racket symbols for names.

; This isn't the first few weeks of CSC108. You're months away from industry or graduate school,
;  so you automatically and systematically generate examples of such expressions, to make the rest of
;  this description concrete and meaningful. You also do this because you follow industry standard
;  Test Driven Development.
; We're still happy to help you develop these skills if you've managed to avoid that until now, but
;  can't indulge requests that actively avoid learning those skills where they are required ---
;  that's a waste of everyone's time in a course that is creating a modern compiler for a modern
;  language down to a real architecture.

#| Semantics of language L0 |#
#;(L0: λ (<id>) <e>)  ; Unary closure creation.
#;(L0: app <e1> <e2>) ; Unary function call.
#;(L0: var <id>)      ; Variable access.
#;(L0: datum <i>)     ; Integer constant.
#;(L0: set! <id> <e>) ; Variable mutation: set <id> to the result of <e>.


#| Language L1 |#

; In the following, <n> is a natural number.
#;(L1: λ <n> <e>)     ; The nth lambda in an L1 expression.
#;(L1: app <e1> <e2>) ; Same meaning as in L0.
#;(L1: var <n>)       ; Reference to a variable <n> scopes up.
#;(L1: var <id>)      ; Free/unbound/open variable reference to <id>.
#;(L1: set! <n> <e>)  ; Set the variable <n> scopes up to the value of <e>.
#;(L1: datum <i>)     ; Same meaning as in L0.


#| DeBruijn Indexing of an L0 Expression

 Replaces each variable referencing a parameter, with a natural number indicating how many scopes up
  the variable is. Free variables are left alone: in later passes they will turn into references to
  pre-defined functions.

 Leaves the form of other L0 expressions alone, but does rewrite their sub-expressions.

 The optional argument env is a simple symbol table: it is the list of all ancestor parameters,
  with their positions being an implicit mapping to how far up in scope they are. |#


(module+ test
  
  (check-equal? (debruijn '(L0: datum 2)) '(L0: datum 2))
  (check-equal? (debruijn '(L0: datum 2) '()) '(L0: datum 2))
  (check-equal? (debruijn '(L0: datum 2) '(x y z)) '(L0: datum 2))
  
  (check-equal? (debruijn '(L0: var y)) '(L0: var y))
  (check-equal? (debruijn '(L0: var y) (list)) '(L0: var y))
  (check-equal? (debruijn '(L0: var y) '(x z)) '(L0: var y))
  (check-equal? (debruijn '(L0: var y) '(x y z)) '(L0: var 1))

  (check-equal? (debruijn '(L0: app (L0: var y) (L0: var x))) '(L0: app (L0: var y) (L0: var x)))
  (check-equal? (debruijn '(L0: app (L0: var y) (L0: var x)) '(x y)) '(L0: app (L0: var 1) (L0: var 0)))

  (check-equal? (debruijn '(L0: set! x (L0: datum 2)) '(y z x)) '(L0: set! 2 (L0: datum 2)))
  (check-equal? (debruijn '(L0: set! x (L0: var y)) '(y x)) '(L0: set! 1 (L0: var 0)))
  
  (check-equal? (debruijn '(L0: λ (x) (L0: datum 2))) '(L0: λ (x) (L0: datum 2)))
  (check-equal? (debruijn '(L0: λ (x) (L0: datum 2)) '(a b c x)) '(L0: λ (x) (L0: datum 2)))
  (check-equal? (debruijn '(L0: λ (x) (L0: var x))) '(L0: λ (x) (L0: var 0)))
  (check-equal? (debruijn '(L0: λ (x) (L0: λ (y) (L0: set! x (L0: var y)))))
                '(L0: λ (x) (L0: λ (y) (L0: set! 1 (L0: var 0))))))

(define (debruijn e [env '()]) ; Takes an optional second argument, which defaults to the empty list.
  (match e
    [`(L0: var ,<id>) (cond [(index-of env <id>) `(L0: var ,(index-of env <id>))]
                                         [else `(L0: var ,<id>)])]
    [`(L0: app ,<e1> ,<e2>) `(L0: app ,(debruijn <e1> env) ,(debruijn <e2> env))]
    [`(L0: set! ,<id> ,<e>) (cond [(index-of env <id>) `(L0: set! ,(index-of env <id>) ,(debruijn <e> env))]
                                      [else `(L0: set! ,<id> ,(debruijn <e>))])]
    [`(L0: λ (,<id>) ,<e>) `(L0: λ (,<id>) ,(debruijn <e> (append (list <id>) env)))]
    [_  e]
))


#| Indexing λs of a Debruijnized L0 Expression

 Replaces each L0 λ with an L1 λ, replacing the parameter list with a numeric index.
 Indexing starts at the value produced by the optional counter argument count, and is applied
  post-order when considering the expression as a tree. |#

; A class to make counting objects.
(define ((counter [c 0]))
  (set! c (add1 c))
  (sub1 c))

(module+ test
  (define c (counter))
  (check-equal? (c) 0)
  (check-equal? (c) 1)
  (define c′ (counter))
  (check-equal? (c′) 0)
  (check-equal? (c) 2))


(module+ test
  (check-equal? (index-λs '(L0: var x)) '(L0: var x))
  (check-equal? (index-λs '(L0: λ 2 (L0: datum 2))) '(L0: λ 2 (L0: datum 2)))
  
  (define d (counter))
  (check-equal? (index-λs '(L0: λ (x) (L0: datum 2)) d) '(L0: λ 0 (L0: datum 2)))
  
  (define d1 (counter))
  (check-equal? (index-λs '(L0: app (L0: λ (x) (L0: datum 2)) (L0: λ (x) (L0: datum 3))) d1)
                '(L0: app (L0: λ 0 (L0: datum 2)) (L0: λ 1 (L0: datum 3))))
  
  (define d2 (counter))
  (check-equal? (index-λs '(L0: λ (x) (L0: app (L0: λ (x) (L0: var 0)) (L0: λ (x) (L0: var 0)))) d2)
                '(L0: λ 2 (L0: app (L0: λ 0 (L0: var 0)) (L0: λ 1 (L0: var 0)))))

  (define d3 (counter))
  (d3)
  (d3)
  (check-equal? (index-λs '(L0: λ (x) (L0: app (L0: λ (x) (L0: var 0)) (L0: λ (x) (L0: var 0)))) d3)
                '(L0: λ 4 (L0: app (L0: λ 2 (L0: var 0)) (L0: λ 3 (L0: var 0))))
  ))

; For a debruijned L0 expression, give each λ expression in e a unique index.
(define (index-λs e [count (counter)])
  (match e
    [`(L0: λ (,<n>) ,<e>) (let ([temp (index-λs <e> count)]) `(L0: λ ,(count) ,temp))]
    [`(L0: app ,<e1> ,<e2>) `(L0: app ,(index-λs <e1> count) ,(index-λs <e2> count))]
    [`(L0: set! ,<n> ,<e>) `(L0: set! ,<n> ,(index-λs <e> count))]
    [_ e]))


#| L0→L1

 For an L0 expression: debruijnizes, indexes λs, and replaces remaining ‘L0:’ tags with ‘L1:’. |#

(module+ test
  (check-equal? (L0→L1 '(L0: datum 488)) '(L1: datum 488))
  (check-equal? (L0→L1 '(L0: var x)) '(L1: var x))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: var x))) '(L1: λ 0 (L1: var 0)))
  (check-equal? (L0→L1 '(L0: set! x (L0: datum 488)))
                '(L1: set! x (L1: datum 488)))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: set! x (L0: datum 488))))
                '(L1: λ 0 (L1: set! 0 (L1: datum 488))))
  (check-equal? (L0→L1 '(L0: app (L0: var x) (L0: var y)))
                '(L1: app (L1: var x) (L1: var y)))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: set! x (L0: datum 488))))
                '(L1: λ 0 (L1: set! 0 (L1: datum 488))))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: app (L0: λ (y) (L0: var y)) (L0: λ (z) (L0: datum 488)))))
                '(L1: λ 2 (L1: app (L1: λ 0 (L1: var 0)) (L1: λ 1 (L1: datum 488))))) 
  (check-equal? (L0→L1 '(L0: app (L0: app (L0: var +) (L0: datum 3)) (L0: datum 4)))
                '(L1: app (L1: app (L1: var +) (L1: datum 3)) (L1: datum 4)))
  (check-equal? (L0→L1 '(L0: app (L0: app (L0: var *) (L0: datum 3)) (L0: datum 4)))
                '(L1: app (L1: app (L1: var *) (L1: datum 3)) (L1: datum 4)))
  (check-equal? (L0→L1 '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var +) (L0: datum 3)) (L0: var x))) (L0: datum 5)))
                '(L1: app (L1: λ 0 (L1: app (L1: app (L1: var +) (L1: datum 3)) (L1: var 0))) (L1: datum 5)))
  )

(define (L0→L1 e)
  (define (L0→L1′ e)
    (match e
      [`(L0: var ,<n>) `(L1: var ,<n>)]
      [`(L0: app ,<e1> ,<e2>) `(L1: app ,(L0→L1′ <e1>) ,(L0→L1′ <e2>))]
      [`(L0: datum ,<i>) `(L1: datum ,<i>)]
      [`(L0: set! ,<n> ,<e>) `(L1: set! ,<n> ,(L0→L1′ <e>))]
      [`(L0: λ ,<n> ,<e>) `(L1: λ ,<n> ,(L0→L1′ <e>))]))
  (L0→L1′ (index-λs (debruijn e))))
