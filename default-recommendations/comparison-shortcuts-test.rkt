#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations comparison-shortcuts


header:
------------------------------
#lang racket/base
(define x 1)
(define y 2)
------------------------------


test: "(> (- x y) 0) refactorable to direct comparison"
- (> (- x y) 0)
- (> x y)


test: "(< (- x y) 0) refactorable to direct comparison"
- (< (- x y) 0)
- (< x y)


test: "(>= (- x y) 0) refactorable to direct comparison"
- (>= (- x y) 0)
- (>= x y)


test: "(<= (- x y) 0) refactorable to direct comparison"
- (<= (- x y) 0)
- (<= x y)


test: "(> 0 (- x y)) refactorable to direct comparison"
- (> 0 (- x y))
- (<= x y)


test: "(< 0 (- x y)) refactorable to direct comparison"
- (< 0 (- x y))
- (>= x y)


test: "(>= 0 (- x y)) refactorable to direct comparison"
- (>= 0 (- x y))
- (< x y)


test: "(<= 0 (- x y)) refactorable to direct comparison"
- (<= 0 (- x y))
- (> x y)
