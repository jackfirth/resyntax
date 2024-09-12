#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations require-and-provide-suggestions


header:
- #lang racket/base


test: "duplicate provided identifiers should be removed"
----------------------------------------
(provide foo
         foo
         foo)
(define foo 1)
----------------------------------------
----------------------------------------
(provide foo)
(define foo 1)
----------------------------------------


test: "removing duplicate provided identifiers leaves other exports unchanged"
----------------------------------------
(provide a
         foo
         b
         foo
         c)
(define a 1)
(define b 1)
(define c 1)
(define foo 1)
----------------------------------------
----------------------------------------
(provide a
         foo
         b
         c)
(define a 1)
(define b 1)
(define c 1)
(define foo 1)
----------------------------------------


test: "provide deduplication doesn't affect exports at different phases"
----------------------------------------
(provide foo
         (for-syntax foo))
(require (for-syntax racket/base))
(define foo 1)
(begin-for-syntax
  (define foo 2))
----------------------------------------