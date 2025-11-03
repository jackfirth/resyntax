#lang resyntax/test


require: resyntax/default-recommendations/analyzers/function-expression-analyzer function-expression-analyzer
header: - #lang racket/base


analysis-test: "applied functions should be annotated"
--------------------
(define (f)
  (list 1 2 3))
--------------------
@within - (list 1 2 3)
@inspect - list
@property application-subexpression-kind
@assert function


analysis-test: "applied function arguments should be annotated"
--------------------
(define (f x y z)
  (list x y z))
--------------------
@within - (list x y z)
@inspect - y
@property application-subexpression-kind
@assert argument
