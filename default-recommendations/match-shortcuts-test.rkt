#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations match-shortcuts


header:
------------------------------
#lang racket/base
(require racket/match)
------------------------------


test: "single-clause match expressions can be replaced with match-define expressions"
------------------------------
(define (foo x)
  (displayln "foo?")
  (match x
    [(list a b c)
     (displayln "foo!")
     (+ a b c)]))
------------------------------
------------------------------
(define (foo x)
  (displayln "foo?")
  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "migrating single-clause match expressions to match-define doesn't reformat context"
------------------------------
(define (foo x)

  (  displayln "foo?"   )

  (match x
    [(list a b c)
     (displayln "foo!")
     (+ a b c)]))
------------------------------
------------------------------
(define (foo x)

  (  displayln "foo?"   )

  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "migrating single-clause match expressions in single-form contexts does reformat"
------------------------------
(map (λ (x) (match x [(list a b c) (+ a b c)]))
     (list (list 1 2 3) (list 4 5 6)))
------------------------------
------------------------------
(map (λ (x)
       (match-define (list a b c) x)
       (+ a b c))
     (list (list 1 2 3) (list 4 5 6)))
------------------------------


test: "single-clause match expressions inside cond can be replaced with match-define expressions"
------------------------------
(define (foo x condition)
  (cond
    [condition
     (displayln "foo?")
     (match x
       [(list a b c)
        (displayln "foo!")
        (+ a b c)])]
    [else (displayln "else")]))
------------------------------
------------------------------
(define (foo x condition)
  (cond
    [condition
     (displayln "foo?")
     (match-define (list a b c) x)
     (displayln "foo!")
     (+ a b c)]
    [else (displayln "else")]))
------------------------------
