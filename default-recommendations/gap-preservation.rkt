#lang racket/base


;; These aren't real refactoring rules. They're only used for testing Resyntax. Specifically, they're
;; used to test that Resyntax properly preserves comments in between sequences of forms that are left
;; unchanged by a refactoring rule, even when forms before and after that sequence are changed. See
;; the accompanying tests in gap-preservation-test.rkt for examples.


(require racket/contract/base)


(provide
 (contract-out
  [gap-preservation-rules refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base
         resyntax/private/syntax-neighbors)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule suggest-inserting-foo-first
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-first)
  (insert-foo-first a ...)
  ("foo" a ...))


(define-refactoring-rule suggest-inserting-foo-second
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-second)
  (insert-foo-second a0 a ...)
  (a0 "foo" a ...))


(define-refactoring-rule suggest-inserting-foo-last
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-last)
  (insert-foo-last a ...)
  (a ... "foo"))


(define-refactoring-rule suggest-inserting-foo-first-and-last
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (insert-foo-first-and-last)
  (insert-foo-first-and-last a ...)
  ("foo" a ... "foo"))


(define-refactoring-rule suggest-replacing-first-with-foo
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (replace-first-with-foo)
  (replace-first-with-foo old a ...)
  ((~replacement "foo" #:original old) a ...))


(define-refactoring-rule suggest-replacing-second-with-foo
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (replace-second-with-foo)
  (replace-second-with-foo a0 old a ...)
  (a0 (~replacement "foo" #:original old) a ...))


(define-refactoring-rule suggest-replacing-last-with-foo
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (replace-last-with-foo)
  (replace-last-with-foo a ... old)
  (a ... (~replacement "foo" #:original old)))


(define-refactoring-rule suggest-replacing-first-and-last-with-foo
  #:description "This refactoring rule is for testing Resyntax, ignore its suggestions."
  #:datum-literals (replace-first-and-last-with-foo)
  (replace-first-and-last-with-foo old1 a ... old2)
  ((~replacement "foo" #:original old1) a ... (~replacement "foo" #:original old2)))


(define-refactoring-suite gap-preservation-rules
  #:rules (suggest-inserting-foo-first
           suggest-inserting-foo-second
           suggest-inserting-foo-last
           suggest-inserting-foo-first-and-last
           suggest-replacing-first-with-foo
           suggest-replacing-second-with-foo
           suggest-replacing-last-with-foo
           suggest-replacing-first-and-last-with-foo))
