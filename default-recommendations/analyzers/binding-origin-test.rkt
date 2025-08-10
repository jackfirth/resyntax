#lang resyntax/test


require: resyntax/default-recommendations default-recommendations
header: - #lang racket/base


analysis-test: "local variable use marked as originating from a lexical binding"
--------------------
(let ([x 1])
  (void x))
--------------------
@within - (void x)
@inspect - x
@property binding-origin
@assert lexical
