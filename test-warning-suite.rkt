#lang racket/base

(require resyntax/base)

(provide test-warning-suite)

;; Define a warning-only rule that matches any (equal? x y)
(define-refactoring-rule test-warning-rule
  #:description "Test warning rule for equal?"
  #:suggested-fixes 'none
  #:literals (equal?)
  (equal? x y)
  (void))

(define test-warning-suite
  (refactoring-suite #:rules (list test-warning-rule)))
