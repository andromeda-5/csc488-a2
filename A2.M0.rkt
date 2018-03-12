#lang racket #| Macros and Runtime Defining Language M0 |#

(provide T:*id* T:*datum*
         T:set! T:if
         T:λ T:*app*
         T:block
         T:let T:local
         T:cond T:when T:while
         T:breakable T:continuable T:returnable
         T:and T:or
         Ts ; List of all the transformations, defined after all of them.
         standard-library
         M0→L0)

(require "A2.L0.rkt")

; Compile an M0 expression to an L0 expression.
(define (M0→L0 e)
  (expand (standard-library e) Ts))

#| Language M0
   ===========

 M0 is really a language and standard library: the language is essentially an extension of L0,
  that macro-compiles to L0, along with a small standard library written in that language.

 M0 is meant to be for humans to write programs in, so we won't tag it.

 There are seventeen kinds of expression, corresponding to the provides of the form ‘T:<id>’ above.
 For each of those, except T:*id*, T:*datum*, and T:*app*, there's an M0 expression (<id> <part> ...).

 Function application is then any other expression of the form: (<part> ...).
   During expansion, the macro system turns that into (*app* <id>), for T:*app* to transform.

 A simple identifier is an M0 expression meaning variable access.
   During expansion, the macro system turns that into (*id* <id>), for T:*id* to transform.

 An integer is an M0 expression meaning a constant.
  During expansion, the macro system turns that into (*datum* <id>), for T:*datum* to transform.

 It's assumed that M0 programmers will not use identifiers surrounded by asterisks. |#

; An M0 expression:
#;(λ (f a b)
    (set! a b)
    (f a 488))
; During expansion, when some parts of that are expanded, they'll be treated as if that was:
#;(λ (f a b)
    (set! a (*id* b))
    (*app* (*id* f) (*id* a) (*datum* 488)))

#| Compiling M0 to L0
   ==================

 Implement:

   1. The transformers mentioned in the ‘provide’ above, for the forms described below.

   2. The standard library, which is a function that attaches the library to an expression,
       described at the end of this file.

 Each transformer ‘T:<id>’ transforms code of the form ‘(<id> <e> ...)’.

 In the given patterns:
   ‘...’  means zero or more of the previous component
   ‘...+’ means one  or more of the previous component

 Other than T:*id*, T:*datum*, T:set!, T:*if*, T:*λ*, and T:*app*, transformers should not
  directly produce L0 forms.

 New identifiers introduced by transformations
 ---------------------------------------------
 In some of your templates you will find that you need to make up the name of an identifier.

 For new “dummy” identifiers that aren't referenced, use ‘_’.
   • we'll assume that user code does not reference any identifier with that name either

 For temporary identifiers that are referenced, use an identifier of the form "*<id>*",
  i.e. surrounded by asterisks. But don't use the names ‘*app*’, ‘*id*’, nor ‘*datum*’.
   • it will be assumed that user code does not use such identifiers |#

(module+ test (require rackunit))

; *id* *datum* set! if
; --------------------
; The following two special cases should expand to integers 0 and 1:
#;(*id* false)
#;(*id* true)
; Otherwise, they are straightforward untagged versions of L0 expressions.
; Transform those directly to their L0 form.

(module+ test
  (check-equal? (transformer-name T:*id*) '*id*)
  (check-equal? ((transformer-function T:*id*) '(*id* false)) 0)
  (check-equal? ((transformer-function T:*id*) '(*id* true)) 1)
  (check-equal? ((transformer-function T:*id*) '(*id* x)) '(L0: var x)))

(define-transformer T:*id* *id*

  [`(*id* false) 0]
  [`(*id* true) 1]
  [`(*id* ,*<id>*) `(L0: var ,*<id>*)])

(module+ test
  (check-equal? (transformer-name T:*datum*) '*datum*)
  (check-equal? ((transformer-function T:*datum*) '(*datum* 488)) `(L0: datum 488)))

(define-transformer T:*datum* *datum*
  [`(*datum* ,*<datum>*) `(L0: datum ,*<datum>*)])

; CHECK SET! FUNCTION SHOULDN'T INCLUDE 'set!'?
(module+ test
  (check-equal? (transformer-name T:set!) 'set!)
  (check-equal? ((transformer-function T:set!) '(set! a 2)) '(L0: set! a 2)))

(define-transformer T:set! set!
  [`(set! ,*<id>* ,*<e>*) `(L0: set! ,*<id>* ,*<e>*)])

(module+ test
  (check-equal? (transformer-name T:if) 'if)
  (check-equal? ((transformer-function T:if) '(if (< 1 2) true false)) '(L0: if (< 1 2) true false)))

(define-transformer T:if if
  [`(if ,*<e1>* ,*<e2>* ,*<e3>*) `(L0: if ,*<e1>* ,*<e2>* ,*<e3>*)]) 

; λ
; -
; Extends L0's λ by:
;   allowing more than one body expression
;   allowing zero parameters, and more than one parameter
;
; Transform within the M0 language by wrapping the body in a ‘block’,
;  adding a dummy parameter if there are zero parameters,
;  and currying if there are two or more parameters.
; Transform the unary single-body-expression form to the L0 form.

(module+ test
  (check-equal? (transformer-name T:λ) 'λ)
  (check-equal? ((transformer-function T:λ) '(λ () (x) (y) (z))) '(λ (_) (block (x) (y) (z))))
  (check-equal? ((transformer-function T:λ) '(λ () (x))) '(λ (_) (block (x))))
  (check-equal? ((transformer-function T:λ) '(λ (x y z) 2)) '(λ (x) (λ (y) (λ (z) 2)))))

(define-transformer T:λ λ
  [`(λ (,*<id>*) ,*<clause>*) `(L0: λ (,*<id>*) *<clause>*)]
  [(list 'λ '() <clause> ..1) `(λ (_) ,(append (list 'block) <clause>))]
  [(list 'λ (list x ..1) <clause> ..1) (let ([tmp (append (list 'λ (list (last x))) <clause>)])
                                         (for ([i (rest (reverse x))])
                                           (set! tmp (list 'λ (list i) tmp)))
                                         tmp)])
               
; *app*
; -----
; Extends L0's app by allowing zero arguments, or more than one argument.
; Transform the form with more than one argument into its curried equivalent.
; Transform the no-argument form into a one-argument form with a dummy argument [see ‘block’].
; Transform the unary form to the L0 form.

;(#%app (lambda (x y) (list x y)) 3 4)
(module+ test
  (check-equal? (transformer-name T:*app*) '*app*)
  (check-equal? ((transformer-function T:*app*) '(*app* (λ (x) (λ (y) 2)) 2 3)) '(((λ (x) (λ (y) 2)) 2) 3))
  (check-equal? ((transformer-function T:*app*) '(*app* (λ (x) (λ (y) (λ (z) 2))) 2 3 4))
                '((((λ (x) (λ (y) (λ (z) 2))) 2) 3) 4)))

(define-transformer T:*app* *app*
  [(list '*app* x y ..1) (let ([tmp x])
                          (for ([i y])
                            (set! tmp (append (list tmp) (list i))))
                          tmp)]
  [(list '*app* x) (list '*app* x `(block))]
  [`(*app* ,<e1> ,<e2>) `(L0: app ,<e1> ,<e2>)])


; block
; -----
#;(block <e>
         ...)
; A sequence of zero or more expressions to be evaluated in order,
;  producing the value of the last expression,
;  or the integer 0 if there are none.
;
; Transform the form with no expressions to the integer 0.
; Transform the form with one expression to just the expression.
; Transform the form with more than one expression to a ‘let’ naming the first expression
;  with a dummy variable.
;
; For other M0 forms that need dummy values [e.g. as mentioned for *app*], use (block) for
;  the dummy value.

(module+ test
  (check-equal? (transformer-name T:block) 'block)
  (check-equal? ((transformer-function T:block) '(block)) 0)
  (check-equal? ((transformer-function T:block) '(block true)) 'true)
  (check-equal? ((transformer-function T:block) '(block (*id* 5) (*datum* 3))) '(let ([<dummy> (*id* 5)]) (*datum* 3)))) 

(define-transformer T:block block
  [`(block) 0]
  [`(block ,<e>) <e>]
  [(list 'block <e> ...) (append (list 'let `([<dummy> ,(first <e>)])) (rest <e>))])


; let
; ---
#;(let ([<id> <init>]
        ...+)
    <body>
    ...+)
; Evaluates the <init>s in order, then introduces the distinctly named local variables <id>s,
;  initialized by the values of the <init>s, then evaluates the <body>s as a block.
;
; Transform using the standard LC transformation: to an expression that makes and immediately calls
;  a function.

(module+ test
  (check-equal? (transformer-name T:let) 'let)
  (check-equal? ((transformer-function T:let) '(let ([<dummy> (*datum* 5)] [<dummy2> (*datum* 7)]) (*datum* 6)))
                '(((λ (<dummy>) (λ (<dummy2>) (block (*datum* 6)))) (*datum* 7)) (*datum* 5))))

(define-transformer T:let let
  [(list 'let (list (list <id> <init>) ..1) <body> ..1)
   (let ([tmp (append (list 'λ (list (last <id>))) (list (append (list 'block) <body>)))])
     (for ([i (rest (reverse <id>))])
       (set! tmp (append (list 'λ (list i)) (list tmp))))
     (for ([j (reverse <init>)])
       (set! tmp (list tmp j)))
     tmp)])
               


; local
; -----
#;(local [(define (<f-id> (<id> ...))
            <f-body>
            ...+)
          ...+]
    <body>
    ...+)
; Introduces the distinctly named local <f-id>s into scope, to functions created in that scope,
;  then evaluates the <body>s as a block.
;
; Transform using the standard LC+set! transformation: to an expression that initializes
;  all the <f-id>s to dummy values, sets them to their functions, then evaluates the body.

(module+ test
  (check-equal? (transformer-name T:local) 'local)
  (check-equal? ((transformer-function T:local) '(local [(define (x (a1 a2)) true) (define (y (a1)) false)] (and (x (1 2)))))
                '(let ((x _) (y _)) (set! x (λ (a1 a2) (true))) (set! y (λ (a1) (false))) (block (and (x (1 2)))))))

(define-transformer T:local local
  [(list 'local (list (list 'define (list <f-id> <args>) <f-body> ..1) ..1) <body> ..1)
   (let ([tmp (list)])
     (for ([i <f-id>])
       (set! tmp (append tmp (list (list i '_)))))
   (set! tmp (append (list 'let) (list tmp)))
     (for ([j <f-id>][k <args>][l <f-body>])
       (set! tmp (append tmp (list `(set! ,j (λ ,k ,l))))))
     (set! tmp (append tmp (list (append (list 'block) <body>))))
     tmp)])

; and or
; ------
#;(and <e0> <e> ...+)
#;(or  <e0> <e> ...+)
; Standard short-circuiting operators for two or more boolean expressions.
; Transform to ‘if’s or ‘cond’s.

(module+ test
  (check-equal? (transformer-name T:and) 'and)
  (check-equal? ((transformer-function T:and) '(and true false)) '(if true (and false) true)))

(define-transformer T:and and
  [(list 'and <e0> <e> ..1) `(if ,<e0> ,(append (list 'and) <e>) ,<e0>)])

(module+ test
  (check-equal? (transformer-name T:or) 'or)
  (check-equal? ((transformer-function T:or) '(or true false)) '(if true true (or false)))
  (check-equal? ((transformer-function T:or) '(or false false false true)) '(if false false (or false false true))))

(define-transformer T:or or
  [(list 'or <e0> <e> ..1) (list 'if <e0> <e0> (append (list 'or) <e>))])


; cond
; ----
#;(cond [<condition> <result>
                     ...+]
        ...+)
#;(cond [<condition> <result>
                     ...+]
        ...
        [else <else-result>
              ...+])
; Evaluates the boolean <condition>s in order, until the first true one or possibly the else,
;  then evaluates the corresponding <result>s as a block.
;
; Transform using ‘if’s, ‘when’s, and/or ‘block’s.
(module+ test
  (check-equal? (transformer-name T:cond) 'cond)
  (check-equal? ((transformer-function T:cond) '(cond (true evaluate nevaluate)))
                '(when (true evaluate nevaluate)))
  (check-equal? ((transformer-function T:cond) '(cond (true evaluate nevaluate) (true2 evalute2)))
                '(if true (block evaluate nevaluate) (cond (true2 evalute2))))
  (check-equal? ((transformer-function T:cond) '(cond (true evaluate nevaluate) (true2 evalute2) (true3 evaluate3)))
                '(if true (block evaluate nevaluate) (cond (true2 evalute2) (true3 evaluate3))))
  (check-equal? ((transformer-function T:cond) '(cond (else 5))) '(block 5))
  (check-equal? ((transformer-function T:cond) '(cond (true e1 e2) (else 5)))
                `(if true (block e1 e2) (block 5))))

(define-transformer T:cond cond
  [(list 'cond (list 'else <else-result> ..1)) (append (list 'block) <else-result>)]
  [(list 'cond (list <condition> <result> ..1) ... (list 'else <else-result> ..1))
   (if (= (length <condition>) 1)
       `(if ,(first <condition>) ,(append (list 'block) (first <result>)) ,(append (list 'block) <else-result>))
       `(if ,(first <condition>) ,(append (list 'block) (first <result>)) ,(let ([tmp (list)])
                                                                               (for ([i (<condition>)][j (rest <result>)])
                                                                                 (set! tmp (append tmp (list (append (list i) j)))))
                                                                               (set! tmp (append (list 'cond) (list tmp)))
                                                                               tmp)))]
  [(list 'cond (list <condition> <result> ..1) ..1)
   (if (= (length <condition>) 1)
         `(when ,(append <condition> (first <result>)))
         `(if ,(first <condition>) ,(append (list 'block) (first <result>)) ,(let ([tmp (list)])
                                                                               (for ([i (rest <condition>)][j (rest <result>)])
                                                                                 (set! tmp (append tmp (list (append (list i) j)))))
                                                                               (set! tmp (append (list 'cond) tmp))
                                                                               tmp)))]) 


; when
; ----
#;(when <condition>
    <body>
    ...+)
; If boolean <condition> is true evaluates the <body>s as a block, otherwise produces a dummy value.

(module+ test
  (check-equal? (transformer-name T:when) 'when)
  (check-equal? ((transformer-function T:when) '(when true 2)) (list 'if 'true '(block 2) (list 'block))))

(define-transformer T:when when
  [(list 'when <condition> <body> ..1) (list 'if <condition> (append (list 'block) <body>) (list 'block))])


; while
; -----
#;(while <condition>
         <body>
         ...+)
; A standard while loop.
; Transform to a recursive no-argument function that is immediately called.
(module+ test
  (check-equal? (transformer-name T:while) 'while)
  (check-equal? ((transformer-function T:while) '(while true 5)) '((λ () (block 5) (while true 5)) (block)))
  (check-equal? ((transformer-function T:while) '(while (< 5 4) (+ 2 3) (+ 4 5)))
                '((λ () (block (+ 2 3) (+ 4 5)) (while (< 5 4) (+ 2 3) (+ 4 5)))(block))))

(define-transformer T:while while
  [(list 'while <condition> <body> ..1) `((λ () ,(append (list 'block) <body>) ,(append (list 'while <condition>) <body>)) (block))])


; returnable breakable continuable
; --------------------------------
#;(returnable <e>
              ...+)
#;(breakable <e>
             ...+)
#;(continuable <e>
               ...+)
; Evaluates the <e>s as a block, in a local scope containing the identifier ‘return’,
;  ‘break’, or ‘continue’ bound to the continuation that escapes the entire expression.
; These are meant to be used manually by the programmer: around a function body, loop, or loop body,
;  to return early, break, or continue.

(define-transformer T:returnable returnable
  [(list 'returnable <e> ..1) (list 'call/ec (list 'λ (list 'return) (append (list 'block) <e>)))])
(define-transformer T:breakable breakable
  [(list 'breakable <e> ..1) (list 'call/ec (list 'λ (list 'break) (append (list 'block) <e>)))])
(define-transformer T:continuable continuable
  [(list 'continuable <e> ..1) (list 'call/ec (list 'λ (list 'continue) (append (list 'block) <e>)))])


; List of all the transformations.
(define Ts (list T:*id* T:*datum*
                 T:set! T:if
                 T:λ T:*app*
                 T:block
                 T:let T:local
                 T:cond T:when T:while
                 T:breakable T:continuable T:returnable
                 T:and T:or))

; Standard Library
; ----------------
; Add definitions for the functions described by the comments in the body.
(define (standard-library e)
  `(local [
           ; Boolean logic
           ; -------------
           ; (not b) : the negation of b, implemented with ‘if’
           (define (not b)
             (if b false true))
           ; Arithmetic
           ; ----------
           ; (- a b) : the difference between a and b
           (define (- a b) (+ a (⊖ b)))
           ; (⊖ a) : the negative of a
           (define (⊖ a) (* -1 a))
           ; (> a b) : whether a is greater than b
           (define (> a b)
             (and (>= a b) (not (= a b))))
           ; (>= a b) : whether a is greater than or equal to b
           (define (>= a b)
             (not (< a b))
           ; (= a b) : whether a is equal to b
           (define (= a b)
             (and (not (< 0 (- a b))) (not (< 0 (- b a)))))
           ]
     ,e))
