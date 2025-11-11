#lang resyntax/test


require: resyntax/default-recommendations let-replacement


header:
- #lang racket/base


test: "expression comments are preserved between let bindings"
------------------------------
(define (foo)
  (let ([x 1]
        #;(debug-binding [z 0])
        [y 2])
    (+ x y)))
==============================
(define (foo)
  (define x 1)
  #;(debug-binding [z 0])
  (define y 2)
  (+ x y))
------------------------------


test: "expression comments with atoms are preserved between let bindings"
------------------------------
(define (foo)
  (let ([x 1]
        #;unused-binding
        [y 2])
    (+ x y)))
==============================
(define (foo)
  (define x 1)
  #;unused-binding
  (define y 2)
  (+ x y))
------------------------------


test: "block comments are preserved between let bindings"
------------------------------
(define (foo)
  (let ([x 1]
        #| The second
           binding |#
        [y 2])
    (+ x y)))
==============================
(define (foo)
  (define x 1)
  #| The second
           binding |#
  (define y 2)
  (+ x y))
------------------------------


test: "expression comments with nested sexps are preserved"
------------------------------
(define (foo)
  (let ([x 1]
        #;(commented-binding
           [y 3]
           [z 4])
        [a 2])
    (+ x a)))
==============================
(define (foo)
  (define x 1)
  #;(commented-binding [y 3] [z 4])
  (define a 2)
  (+ x a))
------------------------------


test: "expression comments in let body are preserved"
------------------------------
(define (foo)
  (let ([x 1])
    #;(debug-stmt)
    (+ x 1)))
==============================
(define (foo)
  (define x 1)
  #;(debug-stmt)
  (+ x 1))
------------------------------


test: "block comments in let body are preserved"
------------------------------
(define (foo)
  (let ([x 1])
    #| computation |#
    (+ x 1)))
==============================
(define (foo)
  (define x 1)
  #| computation |#
  (+ x 1))
------------------------------
