#lang racket/base

(require resyntax/base)

(provide test-warning-suite)

;; Define a warning-only rule that matches any (equal? x y)
(define-refactoring-rule test-warning-rule
  #:description "Test warning rule for equal?"
  #:literals (equal?)
  (equal? x y)
  #:no-suggestion)

(define test-warning-suite
  (refactoring-suite #:rules (list test-warning-rule)))
