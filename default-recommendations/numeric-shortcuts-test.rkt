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
