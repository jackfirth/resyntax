#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations conditional-suggestions


test: "if-else *doesn't* refactor to cond"
------------------------------
#lang racket/base
(if 'cond 'then 'else)
------------------------------


test: "singly-nested if-else *doesn't* refactor to cond"
------------------------------
#lang racket/base
(if 'cond 'then (if 'cond2 'then2 'else))
------------------------------


test: "if-else chain refactors to cond"
------------------------------
#lang racket/base
(if 'a 'b (if 'c 'd (if 'e 'f (if 'g 'h 'i))))
------------------------------
#lang racket/base
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f]
  ['g 'h]
  [else 'i])
------------------------------
