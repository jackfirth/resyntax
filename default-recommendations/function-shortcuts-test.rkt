#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations function-shortcuts


header:
- #lang racket/base


test: "apply with cons can be flattened"
- (apply + 0 (cons 1 '(2 3)))
- (apply + 0 1 '(2 3))


test: "apply with recursive cons can be flattened"
- (apply + 0 (cons 1 (cons 2 '(3 4))))
- (apply + 0 1 2 '(3 4))


test: "apply with list* can be flattened"
- (apply + 0 (list* 1 2 '(3 4)))
- (apply + 0 1 2 '(3 4))


test: "apply with recursive list* can be flattened"
- (apply + 0 (list* 1 2 (list* 3 4 '(5 6))))
- (apply + 0 1 2 3 4 '(5 6))


test: "apply with quasiquoted list can be flattened"
- (apply + 0 `(1 2 ,@'(3 4)))
- (apply + 0 1 2 '(3 4))
