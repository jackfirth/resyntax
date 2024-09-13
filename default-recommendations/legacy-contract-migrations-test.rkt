#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations legacy-contract-migrations


header:
------------------------------
#lang racket/base
(require racket/contract/base)
------------------------------


test: "predicate/c in expression position refactorable to ->"
- (void predicate/c)
- (void (-> any/c boolean?))


test: "predicate/c in contract-out refactorable to ->"
------------------------------
(provide (contract-out [foo? predicate/c]))
(define (foo? _)
  #true)
------------------------------
------------------------------
(provide (contract-out [foo? (-> any/c boolean?)]))
(define (foo? _)
  #true)
------------------------------


test: "predicate/c in define/contract refactorable to ->"
------------------------------
(require racket/contract/region)
(define/contract (foo? _)
  predicate/c
  #true)
------------------------------
------------------------------
(require racket/contract/region)
(define/contract (foo? _)
  (-> any/c boolean?)
  #true)
------------------------------
