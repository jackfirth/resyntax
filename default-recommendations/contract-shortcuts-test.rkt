#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations contract-shortcuts


header:
------------------------------
#lang racket/base
(require racket/contract/base)
------------------------------


test: "nested or/c contracts can be flattened"
- (void (or/c 1 2 (or/c 3 4)))
- (void (or/c 1 2 3 4))


test: "flat or/c contracts can't be flattened"
- (or/c 1 2 3)


test: "multiple nested or/c contracts can be flattened at once"
- (void (or/c (or/c 1 2) (or/c 3 4) (or/c 5 6)))
- (void (or/c 1 2 3 4 5 6))


test: "deeply nested or/c contracts can be flattened in one pass"
- (void (or/c 1 (or/c 2 (or/c 3 (or/c 4 5 6)))))
- (void (or/c 1 2 3 4 5 6))


test: "multiline nested or/c contracts can't be flattened"
------------------------------
(or/c 1
      (or/c 2 3))
------------------------------


test: "nested and/c contracts can be flattened"
- (void (and/c 1 2 (and/c 3 4)))
- (void (and/c 1 2 3 4))


test: "flat and/c contracts can't be flattened"
- (and/c 1 2 3)


test: "multiple nested and/c contracts can be flattened at once"
- (void (and/c (and/c 1 2) (and/c 3 4) (and/c 5 6)))
- (void (and/c 1 2 3 4 5 6))


test: "deeply nested and/c contracts can be flattened in one pass"
- (void (and/c 1 (and/c 2 (and/c 3 (and/c 4 5 6)))))
- (void (and/c 1 2 3 4 5 6))


test: "multiline nested and/c contracts can't be flattened"
------------------------------
(and/c 1
       (and/c 2 3))
------------------------------


test: "nested or/c contracts interspersed with and/c contracts can be flattened"
- (void (or/c (or/c 1 2) (and/c 3 4) (or/c 5 6)))
- (void (or/c 1 2 (and/c 3 4) 5 6))


test: "nested and/c contracts interspersed with or/c contracts can be flattened"
- (void (and/c (and/c 1 2) (or/c 3 4) (and/c 5 6)))
- (void (and/c 1 2 (or/c 3 4) 5 6))


test: "contracts equivalent to predicate/c can be refactored to predicate/c"
- (void (-> any/c boolean?))
- (void predicate/c)


test: "contracts equivalent to path-string? can be refactored to path-string?"
- (void (or/c path? string?))
- (void (or/c string? path?))
- (void path-string?)
