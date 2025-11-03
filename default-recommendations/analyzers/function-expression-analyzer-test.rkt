#lang resyntax/test


require: resyntax/default-recommendations/analyzers/function-expression-analyzer function-expression-analyzer
header: - #lang racket/base


analysis-test: "applied functions should be annotated"
- (list 1 2 3)
@inspect - list
@property application-subexpression-kind
@assert function


analysis-test: "applied function arguments should be annotated"
- (list 1 2 3)
@inspect - 2
@property application-subexpression-kind
@assert argument
