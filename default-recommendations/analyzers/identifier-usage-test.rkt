#lang resyntax/test


require: resyntax/default-recommendations default-recommendations
header: - #lang racket/base


analysis-test: "unused module-level variable"
- (define a 1)
@inspect - a
@property usage-count
@assert 0


analysis-test: "once-used module-level variable"
--------------------
(define a 1)
(void a)
--------------------
@within - (define a 1)
@inspect - a
@property usage-count
@assert 1


analysis-test: "thrice-used module-level variable"
--------------------
(define a 1)
(void a a a)
--------------------
@within - (define a 1)
@inspect - a
@property usage-count
@assert 3


analysis-test: "unused let-bound variable"
--------------------
(let ([a 1])
  (void))
--------------------
@inspect - a
@property usage-count
@assert 0


analysis-test: "once-used let-bound variable"
--------------------
(let ([a 1])
  (void a))
--------------------
@within - [a 1]
@inspect - a
@property usage-count
@assert 1


analysis-test: "thrice-used let-bound variable"
--------------------
(let ([a 1])
  (void a a a))
--------------------
@within - [a 1]
@inspect - a
@property usage-count
@assert 3
