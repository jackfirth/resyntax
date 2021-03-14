#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations boolean-shortcuts


header:
- #lang racket/base


test: "nested ors can be flattened"
- (or 1 2 (or 3 4))
- (or 1 2 3 4)


test: "flat ors can't be flattened"
- (or 1 2 3)


test: "multiple nested ors can be flattened at once"
- (or (or 1 2) (or 3 4) (or 5 6))
- (or 1 2 3 4 5 6)


test: "deeply nested ors can be flattened in one pass"
- (or 1 (or 2 (or 3 (or 4 5 6))))
- (or 1 2 3 4 5 6)


test: "multiline nested ors can't be flattened"
------------------------------
(or 1
    (or 2 3))
------------------------------


test: "nested ands can be flattened"
- (and 1 2 (and 3 4))
- (and 1 2 3 4)


test: "flat ands can't be flattened"
- (and 1 2 3)


test: "multiple nested ands can be flattened at once"
- (and (and 1 2) (and 3 4) (and 5 6))
- (and 1 2 3 4 5 6)


test: "deeply nested ands can be flattened in one pass"
- (and 1 (and 2 (and 3 (and 4 5 6))))
- (and 1 2 3 4 5 6)


test: "multiline nested ands can't be flattened"
------------------------------
(and 1
     (and 2 3))
------------------------------


test: "nested ors interspersed with ands can be flattened"
- (or (or 1 2) (and 3 4) (or 5 6))
- (or 1 2 (and 3 4) 5 6)


test: "nested ands interspersed with ors can be flattened"
- (and (and 1 2) (or 3 4) (and 5 6))
- (and 1 2 (or 3 4) 5 6)


test: "de morgan's law can refactor ands to ors"
- (and (not 1) (not 2) (not 3))
- (not (or 1 2 3))


test: "de morgan's law can refactor ors to ands"
- (or (not 1) (not 2) (not 3))
- (not (and 1 2 3))
