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


test: "using if to convert a boolean expression to a boolean can be removed"
- (if (string? "foo") #true #false)
- (string? "foo")


test: "if else false can be refactored to use and"
- (if (+ 4 10) (* 4 9) #false)
- (and (+ 4 10) (* 4 9))


test: "using if to convert a non-boolean expression can be refactored to use and"
- (if 4 #true #false)
- (and 4 #true)


test: "if then false else true can be refactored to use not"
- (if 4 #false #true)
- (not 4)


test: "when not can be refactored to use unless"
------------------------------
(when (not 'foo)
  (displayln "not foo"))
------------------------------
------------------------------
(unless 'foo
  (displayln "not foo"))
------------------------------


test: "unless not can be refactored to use when"
------------------------------
(unless (not 'foo)
  (displayln "foo"))
------------------------------
------------------------------
(when 'foo
  (displayln "foo"))
------------------------------
