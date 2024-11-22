#lang resyntax/test


require: resyntax/default-recommendations numeric-shortcuts


header:
- #lang racket/base


test: "lambda equivalent to add1 refactorable to add1"
- (map (λ (x) (+ x 1)) (list 1 2 3))
- (map (λ (x) (+ 1 x)) (list 1 2 3))
- (map add1 (list 1 2 3))


test: "lambda equivalent to sub1 refactorable to sub1"
- (map (λ (x) (- x 1)) (list 1 2 3))
- (map (λ (x) (+ x -1)) (list 1 2 3))
- (map (λ (x) (+ -1 x)) (list 1 2 3))
- (map sub1 (list 1 2 3))


test: "comparison equivalent to positive? refactorable to positive?"
- (> 42 0)
- (< 0 42)
- (not (<= 42 0))
- (not (>= 0 42))
- (positive? 42)


test: "comparison equivalent to negative? refactorable to negative?"
- (< 42 0)
- (> 0 42)
- (not (>= 42 0))
- (not (<= 0 42))
- (negative? 42)


test: "lambda equivalent to positive? refactorable to positive?"
- (filter (λ (x) (> x 0)) (list -2 -1 0 1 2))
- (filter (λ (x) (< 0 x)) (list -2 -1 0 1 2))
- (filter (λ (x) (not (<= x 0))) (list -2 -1 0 1 2))
- (filter (λ (x) (not (>= 0 x))) (list -2 -1 0 1 2))
- (filter positive? (list -2 -1 0 1 2))


test: "lambda equivalent to negative? refactorable to negative?"
- (filter (λ (x) (< x 0)) (list -2 -1 0 1 2))
- (filter (λ (x) (> 0 x)) (list -2 -1 0 1 2))
- (filter (λ (x) (not (>= x 0))) (list -2 -1 0 1 2))
- (filter (λ (x) (not (<= 0 x))) (list -2 -1 0 1 2))
- (filter negative? (list -2 -1 0 1 2))


test: "addition of a single term is identity"
- (+ 1)
- 1


test: "multiplication of a single term is identity"
- (* 512)
- 512
