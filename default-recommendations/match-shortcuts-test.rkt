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
  (match x
    [(list a b c)
     (displayln "foo!")
     (+ a b c)]))
------------------------------
------------------------------
(define (foo x)
  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------
