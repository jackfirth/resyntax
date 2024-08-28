#lang racket/base


;; These aren't real refactoring rules. They're only used for testing Resyntax. Specifically, they're
;; used to test that Resyntax properly preserves comments in between sequences of forms that are left
;; unchanged by a refactoring rule, even when forms before and after that sequence are changed. See
;; the accompanying tests in gap-preservation-test.rkt for examples.


(require racket/contract/base)


(provide
 (contract-out
  [gap-preservation-rules refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/private/static-name
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/exception
         resyntax/default-recommendations/private/let-binding
         resyntax/default-recommendations/private/metafunction
         resyntax/default-recommendations/private/syntax-lines
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule suggest-inserting-foo-first
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-first)
  [(insert-foo-first a ...) ("foo" a ...)])


(define-refactoring-rule suggest-inserting-foo-second
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-second)
  [(insert-foo-second a0 a ...) (a0 "foo" a ...)])


(define-refactoring-rule suggest-inserting-foo-last
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-last)
  [(insert-foo-last a ...) (a ... "foo")])


(define-refactoring-rule suggest-inserting-foo-first-and-last
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-first-and-last)
  [(insert-foo-first-and-last a ...) ("foo" a ... "foo")])


(define gap-preservation-rules
  (refactoring-suite
   #:name (name gap-preservation-rules)
   #:rules
   (list suggest-inserting-foo-first
         suggest-inserting-foo-second
         suggest-inserting-foo-last
         suggest-inserting-foo-first-and-last)))