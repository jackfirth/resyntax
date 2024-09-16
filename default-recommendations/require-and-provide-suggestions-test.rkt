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


test: "require tidying sorts collection paths by name"
----------------------------------------
(require racket/string
         racket/hash
         racket/list)
----------------------------------------
----------------------------------------
(require racket/hash
         racket/list
         racket/string)
----------------------------------------



test: "require tidying does nothing when collection paths already sorted by name"
----------------------------------------
(require racket/hash
         racket/list
         racket/string)
----------------------------------------


test: "require tidying removes duplicate imports"
----------------------------------------
(require racket/list
         racket/list)
----------------------------------------
- (require racket/list)


test: "require tidying sorts for-syntax before plain"
----------------------------------------
(require racket/list
         (for-syntax racket/string))
----------------------------------------
----------------------------------------
(require (for-syntax racket/string)
         racket/list)
----------------------------------------


test: "require tidying should move non-phase spec forms to the end"
----------------------------------------
(require (only-in racket/list first)
         (only-in racket/list second)
         (prefix-in s: racket/string)
         racket/hash)
----------------------------------------
----------------------------------------
(require racket/hash
         (only-in racket/list first)
         (only-in racket/list second)
         (prefix-in s: racket/string))
----------------------------------------
