#lang racket #| Compile A2's Language L0 to A2's Language L1 |#

(provide debruijn index L0→L1)
(module+ test (require rackunit))

#| A2's language L0 is A1's language L0 with one additional conditional expression. |#

; If <e1> is true then evaluate <e2>, else evaluate <e3>.
#;(L0: if <e1> <e2> <e3>)

#| A2's language L1 is A1's language L1 with one additional conditional expression. |#

; The nth if expression.
#;(L1: if <n> <e1> <e2> <e3>)

#| DeBruijn Indexing of an L0 Expression

 Replaces each variable referencing a parameter, with a natural number indicating how many scopes up
  the variable is. Free variables are left alone: in later passes they will turn into references to
  pre-defined functions.

 Leaves the form of other L0 expressions alone, but does rewrite their sub-expressions.

 The optional argument env is a simple symbol table: it is the list of all ancestor parameters,
  with their positions being an implicit mapping to how far up in scope they are. 

 This is the same as in A1. |#

; datum tests

(module+ test
  (check-equal? (debruijn′ '(L0: datum 488)) '(L0: datum 488)))

; var tests

(module+ test
  (check-equal? (debruijn′ '(L0: var x)) '(L0: var x))
  (check-equal? (debruijn′ '(L0: var x) '(y z)) '(L0: var x)) 
  (check-equal? (debruijn′ '(L0: var x) '(y x z)) '(L0: var 1))
  (check-equal? (debruijn′ '(L0: var *)) '(L0: var *))
  (check-equal? (debruijn′ '(L0: var +)) '(L0: var +))
  (check-equal? (debruijn′ '(L0: var <)) '(L0: var <)))

; set tests

(module+ test
  (check-equal? (debruijn′ '(L0: set! x (L0: datum 488))) '(L0: set! x (L0: datum 488)))
  (check-equal? (debruijn′ '(L0: set! x (L0: datum 488)) '(y z x))
                '(L0: set! 2 (L0: datum 488))))

; λ tests

(module+ test
  (check-equal? (debruijn′ '(L0: λ (x) (L0: var x))) '(L0: λ (x) (L0: var 0)))
  (check-equal? (debruijn′ '(L0: λ (x) (L0: λ (y) (L0: var x))) '(z))
                '(L0: λ (x) (L0: λ (y) (L0: var 1)))))

; app tests


(define (debruijn′ e [env '()])
  (match e
      [`(L0: λ (,<id>) ,<e>) `(L0: λ (,<id>) ,(debruijn′ <e> (append (list <id>) env)))]
      [`(L0: if ,<e1> , <e2> ,<e3>) '(L0 if ,(debruijn′ <e1>)
                                            ,(debruijn′ <e2>)
                                            ,(debruijn′ <e3>))]
      [`(L0: var ,<id>) (cond [(index-of env <id>) `(L0: var, (index-of env <id>))]
                              [else `(L0: var ,<id>)])]
      [`(L0: app ,<e1> ,<e2>) `(L0: app ,(debruijn′ <e1> env) ,(debruijn′ <e2> env))]
    
      [`(L0: set! ,<id> ,<e>) (cond [(index-of env <id>) `(L0: set! ,(index-of env <id>)
                                                               ,(debruijn′ <e> env))]
                                    [else `(L0: set! ,<id> ,(debruijn′ <e> env))])]
      [_ e]
  ))


(define (debruijn-if e)
  (define c (counter))
  (define (debruijn-if′ e)
    (match e
      [`(L0: if ,<d-e1> ,<d-e2> ,<d-e3>) `(L0: if ,(c) ,(debruijn-if′ <d-e1>)
                                               ,(debruijn-if′ <d-e2>)
                                               ,(debruijn-if′ <d-e3>))]
      [`(L0: λ (,<id>) ,<e>) `(L0: λ (,<id>) ,(debruijn-if′ <e>))]
      [`(L0: app ,<e1> ,<e2>) `(L0: ,(debruijn-if <e1>) ,(debruijn-if <e2>))]
      [`(L0: set! ,<id> ,<e>) `(L0: set! ,<id> ,(debruijn-if <e>))]
      [_ e]))
  (debruijn-if′ e))
  

(define (debruijn e [env '()]) ; Takes an optional second argument, which defaults to the empty list.
  (define e′ (debruijn′ e env))
  (debruijn-if e′)
 )


#|      [`(L0: if ,<e1> , <e2> ,<e3>) (let ()
                                      (define n (cond [(or (empty? env) (symbol? (last env))) (counter)]
                                                      [else (last env)]))
                                      `(L1: ,n
                                            ,(debruijn <e1> (append env (list n)))
                                            ,(debruijn <e2> (append env (list n)))
                                            ,(debruijn <e3> (append env (list n)))))] |#

#| Indexing of a Debruijnized L0 Expression

 For the A1 subset of L0 this is the same.
 The new conditional expressions are also given unique indices. |#

(define ((counter [c 0]))
  (set! c (add1 c))
  (sub1 c))

; For a debruijned L0 expression e, give each λ expression a unique index,
;  and each if expression a unique index.
(define (index e [λ-count (counter)] [if-count (counter)])
  (define (index′ e) (index e λ-count if-count))
  e)


#;(module+ test
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  (check-equal? (L0→L1 '(L0: if (L0:...) (L0: ...) (L0: ...)))
                '(L1: if 0 (L1: ...) (L1: ...) (L1: ...)))
  )

#| L0→L1

 For an L0 expression: debruijnizes, indexes, and replaces remaining ‘L0:’ tags with ‘L1:’. |#


(define (L0→L1 e)
  (define (L0→L1′ e) e)
  (L0→L1′ (index (debruijn e))))
