#lang resyntax/test


require: resyntax/default-recommendations/analyzers/expansion-context-analyzer expansion-context-analyzer
header: - #lang racket/base


analysis-test: "code in a module is in a module context"
- (+ 1 2 3)
@inspect - (+ 1 2 3)
@property expansion-context
@assert module


analysis-test: "function arguments are in an expression context"
- (+ 1 2 3)
@inspect - 2
@property expansion-context
@assert expression


analysis-test: "code in a function body is in an internal definition context"
--------------------
(define (f)
  (+ 1 2 3))
--------------------
@inspect - (+ 1 2 3)
@property expansion-context
@assert internal-definition
