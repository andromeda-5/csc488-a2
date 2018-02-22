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

 This is the same as in A1. |#

(define (debruijn e [env '()]) ; Takes an optional second argument, which defaults to the empty list.
  (define (debruijn′ e) (debruijn e env))
  e)

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

#| L0→L1

 For an L0 expression: debruijnizes, indexes, and replaces remaining ‘L0:’ tags with ‘L1:’. |#

(define (L0→L1 e)
  (define (L0→L1′ e) e)
  (L0→L1′ (index (debruijn e))))
