#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations numeric-shortcuts


header:
- #lang racket/base


test: "(+ x 1) lambda refactorable to add1"
- (map (λ (x) (+ x 1)) (list 1 2 3))
- (map add1 (list 1 2 3))


test: "(+ 1 x) lambda refactorable to add1"
- (map (λ (x) (+ 1 x)) (list 1 2 3))
- (map add1 (list 1 2 3))


test: "(- x 1) lambda refactorable to sub1"
- (map (λ (x) (- x 1)) (list 1 2 3))
- (map sub1 (list 1 2 3))


test: "(+ x -1) lambda refactorable to sub1"
- (map (λ (x) (+ x -1)) (list 1 2 3))
- (map sub1 (list 1 2 3))


test: "(+ -1 x) lambda refactorable to sub1"
- (map (λ (x) (+ -1 x)) (list 1 2 3))
- (map sub1 (list 1 2 3))


test: "(> x 0) lambda refactorable to positive?"
- (filter (λ (x) (> x 0)) (list -2 -1 0 1 2))
- (filter positive? (list -2 -1 0 1 2))


test: "(< 0 x) lambda refactorable to positive?"
- (filter (λ (x) (< 0 x)) (list -2 -1 0 1 2))
- (filter positive? (list -2 -1 0 1 2))


test: "(< x 0) lambda refactorable to negative?"
- (filter (λ (x) (< x 0)) (list -2 -1 0 1 2))
- (filter negative? (list -2 -1 0 1 2))


test: "(> 0 x) lambda refactorable to negative?"
- (filter (λ (x) (> 0 x)) (list -2 -1 0 1 2))
- (filter negative? (list -2 -1 0 1 2))
