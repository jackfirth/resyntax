#lang resyntax/test


require: resyntax/default-recommendations comparison-shortcuts


header:
------------------------------
#lang racket/base
(require resyntax/base)
(define x 1)
(define y 2)
------------------------------


no-change-test: "suppressing comparison rule prevents refactoring"
------------------------------
(resyntax-suppress comparison-of-difference-and-zero-to-direct-comparison
  (> (- x y) 0))
------------------------------


test: "unsuppressed comparison is refactored"
- (> (- x y) 0)
- (> x y)


no-change-test: "specific comparison in symmetrical context can be suppressed"
------------------------------
;; Example from the issue - maintaining visual symmetry
(resyntax-suppress comparison-of-difference-and-zero-to-direct-comparison
  (and (= (- x y) 0)
       (> (- x y) 0)
       (< (- x y) 0)))
------------------------------
