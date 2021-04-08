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


test: "two double comparisons with same subject refactorable to triple < comparison"
- (and (< x 10) (> x -10))
- (and (< x 10) (< -10 x))
- (and (> 10 x) (> x -10))
- (and (> 10 x) (< -10 x))
- (and (> x -10) (< x 10))
- (and (< -10 x) (< x 10))
- (and (> x -10) (> 10 x))
- (and (< -10 x) (> 10 x))
- (< -10 x 10)


test: "two double comparisons with same subject refactorable to triple <= comparison"
- (and (<= x 10) (>= x -10))
- (and (<= x 10) (<= -10 x))
- (and (>= 10 x) (>= x -10))
- (and (>= 10 x) (<= -10 x))
- (and (>= x -10) (<= x 10))
- (and (<= -10 x) (<= x 10))
- (and (>= x -10) (>= 10 x))
- (and (<= -10 x) (>= 10 x))
- (<= -10 x 10)
