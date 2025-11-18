#lang resyntax/test


require: resyntax/default-recommendations simplify-named-let-initialization


header:
--------------------
#lang racket
(define (a) 1)
(define (b) 2)
(define (c) 3)
--------------------

test: "original code should be refactorable to new code"
--------------------
(define (f a b c)
  (let loop ([x (+ 1 2 3)]
             [y (if (a)
                    (b)
                    (c))])
    (loop x y)))
====================
(define (f a b c)
  (define init-y
    (if (a)
        (b)
        (c)))
  (let loop ([x (+ 1 2 3)]
             [y init-y])
    (loop x y)))
--------------------


no-change-test: "code not refactorable when side-effecting expression is present"
--------------------
(define (f a b c)
  (let loop ([x (displayln "foo")]
             [y (if (a)
                    (b)
                    (c))])
    (loop x y)))
--------------------


no-change-test: "code not refactorable when all expressions are single-line"
--------------------
(define (f a b c)
  (let loop ([x (+ 1 2 3)]
             [y 42])
    (loop x y)))
--------------------
