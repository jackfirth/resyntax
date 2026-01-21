#lang resyntax/test


require: resyntax/test-warning-suite test-warning-suite


comment-only-test: "warning-only rule should produce a comment"
--------------------
#lang racket/base

(define a 5)
(equal? a a)
--------------------
@inspect - (equal? a a)
@assertMatch test-warning-rule
