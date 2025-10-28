#lang resyntax/test


require: resyntax/default-recommendations let-replacement


header:
- #lang racket/base


test: "nested let bindings"
------------------------------
(define (f)
  (let ([x 1])
    (let ([y 1])
      (let ([z 1])
        (+ x y z)))))
==============================
(define (f)
  (define x 1)
  (define y 1)
  (define z 1)
  (+ x y z))
------------------------------


test: "nested let bindings with interleaved expressions"
------------------------------
(define (f)
  (let ([x 1])
    (displayln "foo")
    (let ([y 1])
      (displayln "bar")
      (let ([z 1])
        (+ x y z)))))
==============================
(define (f)
  (define x 1)
  (displayln "foo")
  (define y 1)
  (displayln "bar")
  (define z 1)
  (+ x y z))
------------------------------


test: "nested conflicting let bindings only partially refactorable"
------------------------------
(define (f)
  (let ([x 1])
    (displayln x)
    (let ([x 2])
      x)))
==============================
(define (f)
  (define x 1)
  (displayln x)
  (let ([x 2]) x))
------------------------------
