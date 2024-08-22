#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations definition-shortcuts


header:
- #lang racket/base


test: "define-values with values refactorable to separate definitions"
------------------------------
(define (foo)
  (define-values (a b c) (values 1 2 3))
  (+ a b c))
------------------------------
------------------------------
(define (foo)
  (define a 1)
  (define b 2)
  (define c 3)
  (+ a b c))
------------------------------
