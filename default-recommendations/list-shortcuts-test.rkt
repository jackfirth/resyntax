#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations list-shortcuts


test: "car reverse of list not refactorable to last of list, due to imports (see issue #11)"
------------------------------
#lang racket/base
(car (reverse (list 1 2 3)))
------------------------------


test: "first reverse of list refactorable to last of list"
------------------------------
#lang racket/base
(require racket/list)
(first (reverse (list 1 2 3)))
------------------------------
#lang racket/base
(require racket/list)
(last (list 1 2 3))
------------------------------


test: "list eq? to quoted empty list refactorable to null? check"
------------------------------
#lang racket/base
(eq? (list 1 2 3) '())
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list eqv? to quoted empty list refactorable to null? check"
------------------------------
#lang racket/base
(eqv? (list 1 2 3) '())
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list equal? to quoted empty list refactorable to null? check"
------------------------------
#lang racket/base
(equal? (list 1 2 3) '())
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list eq? to (list) refactorable to null? check"
------------------------------
#lang racket/base
(eq? (list 1 2 3) (list))
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list eqv? to (list) refactorable to null? check"
------------------------------
#lang racket/base
(eqv? (list 1 2 3) (list))
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list equal? to (list) refactorable to null? check"
------------------------------
#lang racket/base
(equal? (list 1 2 3) (list))
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list eq? to null refactorable to null? check"
------------------------------
#lang racket/base
(eq? (list 1 2 3) null)
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list eqv? to null refactorable to null? check"
------------------------------
#lang racket/base
(eqv? (list 1 2 3) null)
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "list equal? to null refactorable to null? check"
------------------------------
#lang racket/base
(equal? (list 1 2 3) null)
------------------------------
#lang racket/base
(null? (list 1 2 3))
------------------------------


test: "(list) refactorable to '() check"
------------------------------
#lang racket/base
(list)
------------------------------
#lang racket/base
'()
------------------------------
