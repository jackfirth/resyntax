#lang resyntax/test


require: resyntax/default-recommendations class-shortcuts


header:
--------------------
#lang racket/base
(require racket/class)
--------------------


test: "nested send expressions refactorable to flat send+ expression"
--------------------
(define (f obj x y z)
  (send (send (send obj m1 x) m2 y) m3 z))
--------------------
--------------------
(define (f obj x y z)
  (send+ obj (m1 x) (m2 y) (m3 z)))
--------------------


test: "two-method nested send expression not refactorable to send+"
--------------------
(define (f obj x y)
  (send (send obj m1 x) m2 y))
--------------------
