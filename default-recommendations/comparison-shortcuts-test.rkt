#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations comparison-shortcuts


header:
------------------------------
#lang racket/base
(define x 1)
(define y 2)
------------------------------


test: "comparison of difference to zero refactorable to direct > comparison"
- (> (- x y) 0)
- (<= 0 (- x y))
- (> x y)


test: "comparison of difference to zero refactorable to direct < comparison"
- (< (- x y) 0)
- (>= 0 (- x y))
- (< x y)


test: "comparison of difference to zero refactorable to direct >= comparison"
- (< 0 (- x y))
- (>= (- x y) 0)
- (>= x y)


test: "comparison of difference to zero refactorable to direct <= comparison"
- (> 0 (- x y))
- (<= (- x y) 0)
- (<= x y)


