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


analysis-test: "#lang racket/base function use marked as originating from required module"
- (void)
@inspect - void
@property binding-origin
@assert required-module


analysis-test: "imported function use marked as originating from required module"
--------------------
(require racket/list)
(first '(a b c))
--------------------
@inspect - first
@property binding-origin
@assert required-module


analysis-test: "module variable use marked as originating from surrounding file"
--------------------
(define x 1)
(void x)
--------------------
@within - (void x)
@inspect - x
@property binding-origin
@assert local-module
