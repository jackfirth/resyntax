#lang resyntax/test

require:
resyntax/default-recommendations
class-shortcuts

header:
--------------------
#lang racket/base
(require racket/class)
--------------------

test:
"nested send expressions refactorable to flat send+ expression"
--------------------
(define (f obj x y z)
  (send (send (send obj m1 x) m2 y) m3 z))
====================
(define (f obj x y z)
  (send+ obj (m1 x) (m2 y) (m3 z)))
--------------------

test:
"two-method nested send expression not refactorable to send+"
--------------------
(define (f obj x y)
  (send (send obj m1 x) m2 y))
--------------------

test:
"instantiate without by-name arguments refactorable to make-object"
--------------------
(define (f cls x y z)
  (instantiate cls (x y z)))
====================
(define (f cls x y z)
  (make-object cls x y z))
--------------------

test:
"instantiate without by-position arguments refactorable to new"
--------------------
(define (f cls x y z)
  (instantiate cls ()
    [x x]
    [y y]
    [z z]))
====================
(define (f cls x y z)
  (new cls [x x] [y y] [z z]))
--------------------

test:
"instantiate without any arguments not refactorable"
--------------------
(define (f cls)
  (instantiate cls ()))
--------------------
