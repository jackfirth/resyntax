#lang resyntax/test
require: resyntax/default-recommendations/analyzers/ignored-result-values ignored-result-values-analyzer
header: - #lang racket/base


analysis-test: "non-terminal function bodies are ignored"
--------------------
(define (f)
  (displayln "hi")
  (void))
--------------------
@inspect - (displayln "hi")
@property expression-result
@assert ignored


analysis-test: "terminal function bodies are used"
--------------------
(define (f)
  (displayln "hi")
  (void))
--------------------
@inspect - (void)
@property expression-result
@assert used


analysis-test: "function arguments are used"
- (list (void))
@inspect - (void)
@property expression-result
@assert used


analysis-test: "applied functions are used"
- (list (void))
@inspect - list
@property expression-result
@assert used


analysis-test: "variable definitions use their right hand side"
- (define a (void))
@inspect - (void)
@property expression-result
@assert used


analysis-test: "syntax definitions use their right hand side"
--------------------
(require (for-syntax racket/base))
(define-syntax a (void))
--------------------
@inspect - (void)
@property expression-result
@assert used


analysis-test: "begin0 forms use their initial expression"
--------------------
(begin0 (void)
  (displayln "after"))
--------------------
@inspect - (void)
@property expression-result
@assert used


analysis-test: "begin0 forms ignore their trailing body"
--------------------
(begin0 (void)
  (displayln "after"))
--------------------
@inspect - (displayln "after")
@property expression-result
@assert ignored


analysis-test: "let expressions ignore their non-terminal body forms"
--------------------
(let ()
  (displayln "hi")
  (void))
--------------------
@inspect - (displayln "hi")
@property expression-result
@assert ignored


analysis-test: "let expressions use their terminal body form"
--------------------
(let ()
  (displayln "hi")
  (void))
--------------------
@inspect - (void)
@property expression-result
@assert used
