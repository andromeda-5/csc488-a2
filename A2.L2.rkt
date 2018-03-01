#lang racket #| Compile L1 to sequential language L2 |#

(provide (struct-out compiled:L2) L1→L2)
(module+ test (require rackunit))

#| A2's language L2 is A1's language L2 with three additional expressions. |#

; Jumps to a named statement address, if result is false.
#;(L2: label <name>)      ; Name the current statement location.
#;(L2: jump <name>)       ; Jump/goto a statement location.
#;(L2: jump_false <name>) ; Jump/goto a statement location, when result is false.

#| Compiling L1 to L2 |#

; One additional compilation rule is for L1's new conditional.
#;{(L1: if <n> <e1> <e2> <e3>) →
                               code: {<code-for-e1>
                                      (L2: jump_false else_<n>)
                                      <code-for-e2>
                                      (L2: jump end_<n>)
                                      (L2: label else_<n>)
                                      <code-for-e3>
                                      (L2: label end_<n>)}
                               λs: {<λs-for-e1>
                                    <λs-for-e2>
                                    <λs-for-e3>}}

; A second compilation rule passes through references to an additional function.
#;{(L1: var call/ec) → (L2: closure call_ec)}

#| L1→L2 |#

(struct compiled:L2 (code λs) #:transparent)

; Produce a symbol of the form lambda_<n>.
(require (only-in racket/syntax format-symbol))
(define (lambda_ n)
  (format-symbol "lambda_~a" n))

(module+ test
  (check-equal? (L1→L2 '(L1: var 0)) (compiled:L2 '((L2: variable 0)) '()))
  (check-equal? (L1→L2 '(L1: var 4)) (compiled:L2 '((L2: variable 4)) '()))
  (check-equal? (L1→L2 '(L1: var +)) (compiled:L2 '((L2: closure make_add)) '()))
  (check-equal? (L1→L2 '(L1: var *)) (compiled:L2 '((L2: closure make_multiply)) '()))
  (check-equal? (L1→L2 '(L1: var <)) (compiled:L2 '((L2: closure make_less_than)) '()))
  (check-equal? (L1→L2 '(L1: var call/ec)) (compiled:L2 '((L2: closure call_ec)) '()))
  (check-equal? (L1→L2 '(L1: datum 2))
                (compiled:L2 '((L2: set_result 2))
                             '()))
  (check-equal? (L1→L2 '(L1: set! 0 (L1: datum 2)))
                (compiled:L2 '((L2: set_result 2) (L2: set 0))
                             '()))
  (check-equal? (L1→L2 '(L1: app (L1: datum 2) (L1: datum 3)))
                (compiled:L2 '((L2: set_result 2) (L2: push_result) (L2: set_result 3) (L2: call))
                             '()))
  (check-equal? (L1→L2 '(L1: app (L1: λ 0 (L1: set! 0 (L1: datum 488))) (L1: var 2)))
                (compiled:L2 '((L2: closure lambda_0) (L2: push_result) (L2: variable 2) (L2: call))
                             '((lambda_0 ((L2: set_result 488) (L2: set 0))))))
  (check-equal? (L1→L2 '(L1: λ 0 (L1: datum 2)))
                (compiled:L2 '((L2: closure lambda_0))
                             '((lambda_0 ((L2: set_result 2))))))
  (check-equal? (L1→L2 '(L1: λ 1 (L1: λ 0 (L1: datum 2))))
                (compiled:L2 '((L2: closure lambda_1))
                             '((lambda_1 ((L2: closure lambda_0)))
                               (lambda_0 ((L2: set_result 2))))))
  (check-equal? (L1→L2 '(L1: λ 0 (L1: app (L1: datum 2) (L1: datum 3))))
                (compiled:L2 '((L2: closure lambda_0))
                             '((lambda_0 ((L2: set_result 2) (L2: push_result) (L2: set_result 3) (L2: call))))))
  (check-equal? (L1→L2 '(L1: app (L1: app (L1: var +) (L1: datum 3)) (L1: datum 4)))
                (compiled:L2 '((L2: closure make_add) (L2: push_result) (L2: set_result 3) (L2: call) (L2: push_result) (L2: set_result 4) (L2: call))
                             '()))
  (check-equal? (L1→L2 '(L1: app (L1: app (L1: var *) (L1: datum 3)) (L1: datum 4)))
                (compiled:L2 '((L2: closure make_multiply) (L2: push_result) (L2: set_result 3) (L2: call) (L2: push_result) (L2: set_result 4) (L2: call))
                             '()))
  (check-equal? (L1→L2 '(L1: app (L1: λ 0 (L1: app (L1: app (L1: var +) (L1: datum 3)) (L1: var 0))) (L1: datum 5)))
                (compiled:L2 '((L2: closure lambda_0) (L2: push_result) (L2: set_result 5) (L2: call))
                             '((lambda_0 ((L2: closure make_add) (L2: push_result) (L2: set_result 3) (L2: call) (L2: push_result) (L2: variable 0) (L2: call))))))

  (check-equal? (L1→L2 '(L1: if 0 (L1: app (L1: app (L1: var <) (L1: datum 488)) (L1: datum 0)) (L1: datum 0) (L1: datum 488)))
                (compiled:L2 '((L2: closure make_less_than)
                               (L2: push_result)
                               (L2: set_result 488)
                               (L2: call)
                               (L2: push_result)
                               (L2: set_result 0)
                               (L2: call)
                               (L2: jump_false else_0)
                               (L2: set_result 0)
                               (L2: jump end_0)
                               (L2: label else_0)
                               (L2: set_result 488)
                               (L2: label end_0))
                             '()))

  )

(define (L1→L2 e)
  (match e
    [`(L1: var +) (compiled:L2 '((L2: closure make_add)) '())]
    [`(L1: var *) (compiled:L2 '((L2: closure make_multiply)) '())]
    [`(L1: var <) (compiled:L2 '((L2: closure make_less_than)) '())]
    [`(L1: var call/ec) (compiled:L2 '((L2: closure call_ec)) '())]
    [`(L1: var ,<n>) (compiled:L2 `((L2: variable ,<n>)) '())]
    [`(L1: datum ,<i>) (compiled:L2 `((L2: set_result ,<i>)) '())]
    [`(L1: set! ,<n> ,<e>) (let ([e (L1→L2 <e>)])
                             (compiled:L2
                              (append (compiled:L2-code e) `((L2: set ,<n>)))
                              (compiled:L2-λs e)))]
    [`(L1: app ,<e1> ,<e2>) (let ([e1 (L1→L2 <e1>)] [e2 (L1→L2 <e2>)])
                              (compiled:L2
                               (append (compiled:L2-code e1) (list '(L2: push_result)) (compiled:L2-code e2) (list '(L2: call)))
                               (append (compiled:L2-λs e1) (compiled:L2-λs e2))))]
    [`(L1: λ ,<n> ,<e>) (let ([e (L1→L2 <e>)])
                             (compiled:L2
                              `((L2: closure ,(lambda_ <n>)))
                              (cons (list (lambda_ <n>) (compiled:L2-code e)) (compiled:L2-λs e))))]
    [`(L1: if ,<n> ,<e1> ,<e2> ,<e3>) (let ([e1 (L1→L2 <e1>)] [e2 (L1→L2 <e2>)] [e3 (L1→L2 <e3>)])
                                       (compiled:L2
                                        (append (compiled:L2-code e1)
                                                (list `(L2: jump_false ,(format-symbol "else_~a" <n>)))
                                                (compiled:L2-code e2)
                                                (list `(L2: jump ,(format-symbol "end_~a" <n>)))
                                                (list `(L2: label ,(format-symbol "else_~a" <n>)))
                                                (compiled:L2-code e3)
                                                (list `(L2: label ,(format-symbol "end_~a" <n>))))
                                        (append (compiled:L2-λs e1) (compiled:L2-λs e2) (compiled:L2-λs e3))))]                 
    )
  )
