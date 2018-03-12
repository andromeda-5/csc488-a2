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
  (check-equal? (debruijn '(L0: datum 488)) '(L0: datum 488)))

; var tests

(module+ test
  (check-equal? (debruijn '(L0: var x)) '(L0: var x))
  (check-equal? (debruijn '(L0: var x) '(y z)) '(L0: var x)) 
  (check-equal? (debruijn '(L0: var x) '(y x z)) '(L0: var 1))
  (check-equal? (debruijn '(L0: var *)) '(L0: var *))
  (check-equal? (debruijn '(L0: var +)) '(L0: var +))
  (check-equal? (debruijn '(L0: var <)) '(L0: var <)))

; set tests

(module+ test
  (check-equal? (debruijn '(L0: set! x (L0: datum 488))) '(L0: set! x (L0: datum 488)))
  (check-equal? (debruijn '(L0: set! x (L0: datum 488)) '(y z x))
                '(L0: set! 2 (L0: datum 488)))
  (check-equal? (debruijn '(L0: set! x (L0: var y)) '(z y x))
                '(L0: set! 2 (L0: var 1))))


; λ tests

(module+ test
  (check-equal? (debruijn '(L0: λ (x) (L0: var x))) '(L0: λ (x) (L0: var 0)))
  (check-equal? (debruijn '(L0: λ (x) (L0: λ (y) (L0: var x))) '(z))
                '(L0: λ (x) (L0: λ (y) (L0: var 1))))
  (check-equal? (debruijn '(L0: λ (x) (L0: set! x (L0: λ (y) (L0: datum 488)))))
                '(L0: λ (x) (L0: set! 0 (L0: λ (y) (L0: datum 488)))))
  (check-equal? (debruijn '(L0: λ (x) (L0: set! x (L0: λ (y) (L0: var y)))))
                '(L0: λ (x) (L0: set! 0 (L0: λ (y) (L0: var 0)))))
  (check-equal? (debruijn '(L0: λ (x) (L0: set! x (L0: λ (y) (L0: var y)))))
                '(L0: λ (x) (L0: set! 0 (L0: λ (y) (L0: var 0))))))


; app tests
(module+ test
  (check-equal? (debruijn '(L0: app (L0: λ (x) (L0: set! y (L0: var x))) (L0: datum 488)))
                '(L0: app (L0: λ (x) (L0: set! y (L0: var 0))) (L0: datum 488)))
  (check-equal? (debruijn '(L0: app (L0: app (L0: var +) (L0: datum 488)) (L0: datum 0)))
                '(L0: app (L0: app (L0: var +) (L0: datum 488)) (L0: datum 0)))
  (check-equal? (debruijn '(L0: app (L0: app (L0: var *) (L0: datum 488)) (L0: datum 0)))
                '(L0: app (L0: app (L0: var *) (L0: datum 488)) (L0: datum 0)))
  (check-equal? (debruijn '(L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 0)))
                '(L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 0)))

  (check-equal? (debruijn '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var +) (L0: datum 0))(L0: var x)))(L0: datum 488)))
                '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var +) (L0: datum 0)) (L0: var 0))) (L0: datum 488)))
  (check-equal? (debruijn '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var *) (L0: datum 0))(L0: var x)))(L0: datum 488)))
                '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var *) (L0: datum 0)) (L0: var 0))) (L0: datum 488)))
  (check-equal? (debruijn '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var <) (L0: datum 0))(L0: var x)))(L0: datum 488)))
                '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var <) (L0: datum 0)) (L0: var 0))) (L0: datum 488)))
  (check-equal? (debruijn '(L0: app (L0: app (L0: λ (x) (L0: app (L0: λ (y) (L0: app (L0: var +) (L0: var y)))(L0: var x)))(L0: datum 0))(L0: datum 488)))
                '(L0: app (L0: app (L0: λ (x) (L0: app (L0: λ (y) (L0: app (L0: var +) (L0: var 0))) (L0: var 0))) (L0: datum 0)) (L0: datum 488)))
  (check-equal? (debruijn '(L0: app (L0: app (L0: λ (x) (L0: app (L0: λ (y) (L0: app (L0: var *) (L0: var y)))(L0: var x)))(L0: datum 0))(L0: datum 488)))
                '(L0: app (L0: app (L0: λ (x) (L0: app (L0: λ (y) (L0: app (L0: var *) (L0: var 0))) (L0: var 0))) (L0: datum 0)) (L0: datum 488)))
  (check-equal? (debruijn '(L0: app (L0: app (L0: λ (x) (L0: app (L0: λ (y) (L0: app (L0: var <) (L0: var y)))(L0: var x)))(L0: datum 0))(L0: datum 488)))
                '(L0: app (L0: app (L0: λ (x) (L0: app (L0: λ (y) (L0: app (L0: var <) (L0: var 0))) (L0: var 0))) (L0: datum 0)) (L0: datum 488))))


(module+ test
  (check-equal? (debruijn '(L0: if (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: datum 0)) (L0: datum 9) (L0: datum 10)))
                '(L0: if (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: datum 0)) (L0: datum 9) (L0: datum 10)))
                )


(define (debruijn e [env '()]) ; Takes an optional second argument, which defaults to the empty list.
  (match e
    [`(L0: λ (,<id>) ,<e>)
     `(L0: λ (,<id>) ,(debruijn <e> (append (list <id>) env)))]
    [`(L0: if ,<e1> , <e2> ,<e3>)
     `(L0: if ,(debruijn <e1>)
                                       ,(debruijn <e2>)
                                       ,(debruijn <e3>))]
    [`(L0: var ,<id>)
     (cond [(index-of env <id>) `(L0: var, (index-of env <id>))]
                            [else `(L0: var ,<id>)])]
    [`(L0: app ,<e1> ,<e2>)
     `(L0: app ,(debruijn <e1> env) ,(debruijn <e2> env))]
    
    [`(L0: set! ,<id> ,<e>) (cond [(index-of env <id>) `(L0: set! ,(index-of env <id>)
                                                             ,(debruijn <e> env))]
                                  [else `(L0: set! ,<id> ,(debruijn <e> env))])]
    [`(L0: datum ,<n>) `(L0: datum ,<n>)])) 


#| Indexing of a Debruijnized L0 Expression

 For the A1 subset of L0 this is the same.
 The new conditional expressions are also given unique indices. |#

(define ((counter [c 0]))
  (set! c (add1 c))
  (sub1 c))

; For a debruijned L0 expression e, give each λ expression a unique index,
;  and each if expression a unique index.


(module+ test
  (check-equal? (index '(L0: λ (x) (L0: datum 488))) '(L0: λ 0 (x) (L0: datum 488)))
  (check-equal? (index '(L0: λ (x) (L0: λ (y) (L0: λ (z) (L0: datum 488)))))
                '(L0: λ 2 (x) (L0: λ 1 (y) (L0: λ 0 (z) (L0: datum 488))))) )

(module+ test
  (check-equal? (index '(L0: if (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: datum 0)) (L0: datum 488) (L0: datum 0)))
                '(L0: if 0 (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: datum 0)) (L0: datum 488) (L0: datum 0)))
  (check-equal? (index '(L0: if (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: datum 0)) (L0: datum 488) (L0: datum 0)))
                '(L0: if 0 (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: datum 0)) (L0: datum 488) (L0: datum 0)))
  (check-equal? (index '(L0: if (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 0))
                             (L0: if (L0: app (L0: app (L0: var <) (L0: datum 500)) (L0: datum 12))
                                  (L0: app (L0: app (L0: var *) (L0: datum 5)) (L0: datum 100))
                                  (L0: if (L0: app (L0: app (L0: var <) (L0: datum 99)) (L0: datum 401))
                                       (L0: datum 56) (L0: datum 57)))
                             (L0: if (L0: app (L0: app (L0: var <) (L0: datum 6)) (L0: datum 9))
                                  (L0: if (L0: app (L0: app (L0: var <) (L0: datum 7)) (L0: datum 15))
                                        (L0: datum 555) (L0: datum 777))
                                  (L0: if (L0: app (L0: app (L0: var <) (L0: datum 98)) (L0: datum 400))
                                       (L0: datum 999) (L0: datum 1000)))))
                '(L0: if 0 (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 0))
                             (L0: if 1 (L0: app (L0: app (L0: var <) (L0: datum 500)) (L0: datum 12))
                                  (L0: app (L0: app (L0: var *) (L0: datum 5)) (L0: datum 100))
                                  (L0: if 2 (L0: app (L0: app (L0: var <) (L0: datum 99)) (L0: datum 401))
                                       (L0: datum 56) (L0: datum 57)))
                             (L0: if 3 (L0: app (L0: app (L0: var <) (L0: datum 6)) (L0: datum 9))
                                  (L0: if 4 (L0: app (L0: app (L0: var <) (L0: datum 7)) (L0: datum 15))
                                       (L0: datum 555) (L0: datum 777))
                                  (L0: if 5 (L0: app (L0: app (L0: var <) (L0: datum 98)) (L0: datum 400))
                                       (L0: datum 999) (L0: datum 1000))))))

(module+ test
  (check-equal? (index '(L0: app (L0: λ (x) (L0: if (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: var x))
                                                 (L0: datum 500) (L0: datum 501))) (L0: datum 1)))
                '(L0: app (L0: λ 0 (x) (L0: if 0 (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: var x))
                                            (L0: datum 500) (L0: datum 501))) (L0: datum 1)))
  (check-equal? (index '(L0: app (L0: λ (x) (L0: if (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: var x))
                                                 (L0: app (L0: app (L0: var +) (L0: datum 488)) (L0: var x))
                                                 (L0: app (L0: app (L0: var *) (L0: datum 488)) (L0: var x))))
                             (L0: if (L0: app (L0: app (L0: var < )(L0: datum 488)) (L0: datum 488))
                                  (L0: λ (x) (L0: λ (y) (L0: datum 488)))
                                  (L0: λ (x) (L0: λ (y) (L0: datum 489))))))
                '(L0: app (L0: λ 0 (x) (L0: if 0 (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: var x))
                                            (L0: app (L0: app (L0: var +) (L0: datum 488)) (L0: var x))
                                            (L0: app (L0: app (L0: var *) (L0: datum 488)) (L0: var x))))
                      (L0: if 1 (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 488))
                           (L0: λ 2 (x) (L0: λ 1 (y) (L0: datum 488)))
                           (L0: λ 4 (x) (L0: λ 3 (y) (L0: datum 489)))))))

(module+ test
  (check-equal? (index '(L0: set! 0 (L0: var 9)))
                '(L0: set! 0 (L0: var 9)))
  (check-equal? (index '(L0: set! 0 (L0: λ (x) (L0: datum 4))))
                '(L0: set! 0 (L0: λ 0 (x) (L0: datum 4))))
  (check-equal? (index '(L0: set! 0 (L0: if (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 488))
                                         (L0: datum 0)
                                         (L0: datum 488))))
                '(L0: set! 0 (L0: if 0 (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 488))
                                         (L0: datum 0)
                                         (L0: datum 488))))
   (check-equal? (index '(L0: set! 0 (L0: set! 1 (L0: set! 2 (L0: set! 3 (L0: λ (x)
                                                                (L0: if (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 488))
                                                                     (L0: datum 488)
                                                                     (L0: λ (y) (L0: datum 488)))))))))
   '(L0: set! 0 (L0: set! 1 (L0: set! 2 (L0: set! 3 (L0: λ 1 (x)
                                                         (L0: if 0 (L0: app (L0: app (L0: var <) (L0: datum 488)) (L0: datum 488))
                                                              (L0: datum 488)
                                                              (L0: λ 0 (y) (L0: datum 488))))))))
   ))


(define (index e [λ-count (counter)] [if-count (counter)])
  (match e
    [`(L0: app ,<e1> ,<e2>) `(L0: app ,(index <e1> λ-count if-count) ,(index <e2> λ-count if-count))]
    [`(L0: set! ,<n> ,<e>) `(L0: set! ,<n> ,(index <e> λ-count if-count))]
    [`(L0: λ (,<id>) ,<e>) (let ([tmp (index <e> λ-count if-count)]) `(L0: λ ,(λ-count) (,<id>) ,tmp))]
    [`(L0: if ,<e1> ,<e2> ,<e3>) `(L0: if ,(if-count) ,(index <e1> λ-count if-count) ,(index <e2> λ-count if-count) ,(index <e3> λ-count if-count))]
    [_ e]
    ))

;old tests
(module+ test
  (check-equal? (L0→L1 '(L0: datum 488)) '(L1: datum 488))
  (check-equal? (L0→L1 '(L0: var x)) '(L1: var x))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: var x))) '(L1: λ 0 (L1: var 0)))
  (check-equal? (L0→L1 '(L0: set! x (L0: datum 488)))
                '(L1: set! x (L1: datum 488)))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: set! x (L0: datum 488))))
                '(L1: λ 0 (L1: set! 0 (L1: datum 488))))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: set! x (L0: datum 488))))
                '(L1: λ 0 (L1: set! 0 (L1: datum 488))))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: app (L0: λ (y) (L0: var y)) (L0: λ (z) (L0: datum 488)))))
                '(L1: λ 2 (L1: app (L1: λ 0 (L1: var 0)) (L1: λ 1 (L1: datum 488)))))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: var x)))'(L1: λ 0 (L1: var 0)))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: λ (y) (L0: var x)))) ' (L1: λ 1 (L1: λ 0 (L1: var 1))))
  (check-equal? (L0→L1 '(L0: λ (x) (L0: set! x (L0: datum 488))))
                '(L1: λ 0 (L1: set! 0 (L1: datum 488))))
  (check-equal? (L0→L1 '(L0: app (L0: var x) (L0: var y)))
                '(L1: app (L1: var x) (L1: var y)))
  (check-equal? (L0→L1 '(L0: app (L0: app (L0: var +) (L0: datum 3)) (L0: datum 4)))
                '(L1: app (L1: app (L1: var +) (L1: datum 3)) (L1: datum 4)))
  (check-equal? (L0→L1 '(L0: app (L0: app (L0: var *) (L0: datum 3)) (L0: datum 4)))
                '(L1: app (L1: app (L1: var *) (L1: datum 3)) (L1: datum 4)))
  (check-equal? (L0→L1 '(L0: app (L0: λ (x) (L0: app (L0: app (L0: var +) (L0: datum 3)) (L0: var x))) (L0: datum 5)))
                '(L1: app (L1: λ 0 (L1: app (L1: app (L1: var +) (L1: datum 3)) (L1: var 0))) (L1: datum 5)))
  (check-equal? (L0→L1 '(L0: if (L0: app (L0: app (L0: var < ) (L0: datum 488)) (L0: datum 0)) (L0: datum 488) (L0: datum 0)))
                '(L1: if 0 (L1: app (L1: app (L1: var <) (L1: datum 488)) (L1: datum 0)) (L1: datum 488) (L1: datum 0))))


#| L0→L1

 For an L0 expression: debruijnizes, indexes, and replaces remaining ‘L0:’ tags with ‘L1:’. |#

(define (L0→L1 e)
  (define (L0→L1′ e)
    (match e
      [`(L0: datum ,<n>) `(L1: datum ,<n>)]
      [`(L0: var ,<id>) `(L1: var ,<id>)]
      [`(L0: set! ,<id> ,<e>) `(L1: set! ,<id> ,(L0→L1′ <e>))]
      [`(L0: app ,<e1> ,<e2>) `(L1: app ,(L0→L1′ <e1>) ,(L0→L1′ <e2>))]
      [`(L0: λ ,<n> ,<id> ,<e>) `(L1: λ ,<n> ,(L0→L1′ <e>))]
      [`(L0: if ,<n> ,<e1> ,<e2> ,<e3>) `(L1: if ,<n> ,(L0→L1′ <e1>) ,(L0→L1′ <e2>) ,(L0→L1′ <e3>))]))
  (L0→L1′ (index (debruijn e))))
