#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations conditional-shortcuts


header:
------------------------------
#lang racket/base
------------------------------

test: "if without nested ifs not refactorable"
- (if 'cond 'then 'else)


test: "singly-nested ifs refactorable to cond"
- (if 'cond 'then (if 'cond2 'then2 'else))
------------------------------
(cond
  ['cond 'then]
  ['cond2 'then2]
  [else 'else])
------------------------------


test: "one-line nested ifs refactorable to cond"
- (if 'a 'b (if 'c 'd (if 'e 'f (if 'g 'h 'i))))
------------------------------
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f]
  ['g 'h]
  [else 'i])
------------------------------


test: "multi-line nested ifs refactorable to cond"
------------------------------
(if 'a
    'b
    (if 'c
        'd
        (if 'e
            'f
            (if 'g
                'h
                'i))))
------------------------------
------------------------------
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f]
  ['g 'h]
  [else 'i])
------------------------------


test: "if else false can be refactored to an and expression"
- (if 'a (println "true branch") #f)
- (and 'a (println "true branch"))


test: "multi-line if else false can be refactored to a multi-line and expression"
------------------------------
(if 'a
    (println "true branch")
    #f)
------------------------------
------------------------------
(and 'a
     (println "true branch"))
------------------------------


test: "if x else x can be refactored to an and expression"
------------------------------
(define x 'a)
(if x (println "true branch") x)
------------------------------
------------------------------
(define x 'a)
(and x (println "true branch"))
------------------------------


test: "multi-line if x else x can be refactored to a multi-line and expression"
------------------------------
(define x 'a)
(if x
    (println "true branch")
    x)
------------------------------
------------------------------
(define x 'a)
(and x
     (println "true branch"))
------------------------------
