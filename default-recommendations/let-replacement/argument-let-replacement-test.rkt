#lang resyntax/test


require: resyntax/default-recommendations/let-replacement/argument-let-replacement argument-let-replacement


header:
--------------------
#lang racket
(define (g x y)
  (+ x y))
--------------------


test: "let in function argument can be extracted"
--------------------
(define (f)
  (g (let ([x 42])
       (* x 2))
     (let ([y 100])
       (* y 3))))
====================
(define (f)
  (define x 42)
  (define y 100)
  (g (* x 2) (* y 3)))
--------------------


test: "single let in function argument can be extracted"
--------------------
(define (f)
  (list (let ([x 10])
          (+ x 5))))
====================
(define (f)
  (define x 10)
  (list (+ x 5)))
--------------------


test: "multiple bindings in single let can be extracted"
--------------------
(define (f)
  (g (let ([x 1]
           [y 2])
       (+ x y))))
====================
(define (f)
  (define x 1)
  (define y 2)
  (g (+ x y)))
--------------------


test: "let in one argument while other arguments are plain"
--------------------
(define (f)
  (cons (let ([x 42])
          (* x 2))
        100))
====================
(define (f)
  (define x 42)
  (cons (* x 2) 100))
--------------------


test: "nested lets in same argument are extracted"
--------------------
(define (f)
  (list (let ([x 1])
          (let ([y 2])
            (+ x y)))))
====================
(define (f)
  (define x 1)
  (define y 2)
  (list (+ x y)))
--------------------
