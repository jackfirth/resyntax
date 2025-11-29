#lang racket/base

(require racket/list
         resyntax/base
         resyntax
         resyntax/private/refactoring-result
         resyntax/private/source
         rackunit)

;; Define a warning-only rule that matches any (equal? x y)
(define-refactoring-rule test-warning-rule
  #:description "This is a test warning rule for equal?"
  #:literals (equal?)
  (equal? x y)
  #:no-suggestion)

;; Test that the rule works
(define test-suite (refactoring-suite #:rules (list test-warning-rule)))

(define test-source (string-source "#lang racket/base\n(define a 5)\n(equal? a a)\n"))

(define result-set (resyntax-analyze test-source #:suite test-suite))

(define results (refactoring-result-set-results result-set))

(test-case "warning-only rule produces a result"
  (check-equal? (length results) 1 "Should have one result"))

(test-case "warning-only result has no fix"
  (define result (first results))
  (check-false (refactoring-result-has-fix? result) "Should not have a fix")
  (check-false (refactoring-result-syntax-replacement result) "Should have no syntax replacement")
  (check-false (refactoring-result-new-code result) "Should have no new code"))

( test-case "warning-only result has message and location"
  (define result (first results))
  (check-equal? (refactoring-result-message result) "This is a test warning rule for equal?")
  (check-equal? (refactoring-result-rule-name result) 'test-warning-rule)
  (check-true (positive? (refactoring-result-original-line result))))

(test-case "warning-only result doesn't modify source"
  (define updated (refactoring-result-set-updated-source result-set))
  (define updated-contents (modified-source-contents updated))
  (check-equal? updated-contents (source->string test-source)
               "Source should not be modified"))
