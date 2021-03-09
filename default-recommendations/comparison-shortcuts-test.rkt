#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations comparison-shortcuts


test: "(> (- x y) 0) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(> (- x y) 0)
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(> x y)
------------------------------


test: "(< (- x y) 0) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(< (- x y) 0)
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(< x y)
------------------------------


test: "(>= (- x y) 0) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(>= (- x y) 0)
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(>= x y)
------------------------------


test: "(<= (- x y) 0) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(<= (- x y) 0)
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(<= x y)
------------------------------


test: "(> 0 (- x y)) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(> 0 (- x y))
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(<= x y)
------------------------------


test: "(< 0 (- x y)) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(< 0 (- x y))
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(>= x y)
------------------------------


test: "(>= 0 (- x y)) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(>= 0 (- x y))
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(< x y)
------------------------------


test: "(<= 0 (- x y)) refactorable to direct comparison"
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(<= 0 (- x y))
------------------------------
#lang racket/base
(define x 1)
(define y 2)
(> x y)
------------------------------
