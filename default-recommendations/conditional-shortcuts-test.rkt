#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations conditional-shortcuts


test: "if without nested ifs not refactorable"
- #lang racket/base (if 'cond 'then 'else)


test: "singly-nested ifs refactorable to cond"
------------------------------
#lang racket/base
(if 'cond 'then (if 'cond2 'then2 'else))
------------------------------
------------------------------
#lang racket/base
(cond
  ['cond 'then]
  ['cond2 'then2]
  [else 'else])
------------------------------


test: "one-line nested ifs refactorable to cond"
------------------------------
#lang racket/base
(if 'a 'b (if 'c 'd (if 'e 'f (if 'g 'h 'i))))
------------------------------
------------------------------
#lang racket/base
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f]
  ['g 'h]
  [else 'i])
------------------------------


test: "multi-line nested ifs refactorable to cond"
------------------------------
#lang racket/base
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
#lang racket/base
(cond
  ['a 'b]
  ['c 'd]
  ['e 'f]
  ['g 'h]
  [else 'i])
------------------------------


test: "if else false can be refactored to an and expression"
- #lang racket/base (if 'a (println "true branch") #f)
- #lang racket/base (and 'a (println "true branch"))


test: "multi-line if else false can be refactored to a multi-line and expression"
------------------------------
#lang racket/base
(if 'a
    (println "true branch")
    #f)
------------------------------
------------------------------
#lang racket/base
(and 'a
     (println "true branch"))
------------------------------


test: "if x else x can be refactored to an and expression"
------------------------------
#lang racket/base
(define x 'a)
(if x (println "true branch") x)
------------------------------
------------------------------
#lang racket/base
(define x 'a)
(and x (println "true branch"))
------------------------------


test: "multi-line if x else x can be refactored to a multi-line and expression"
------------------------------
#lang racket/base
(define x 'a)
(if x
    (println "true branch")
    x)
------------------------------
------------------------------
#lang racket/base
(define x 'a)
(and x
     (println "true branch"))
------------------------------
