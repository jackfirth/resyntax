#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations list-shortcuts


header:
- #lang racket/base


test: "car reverse of list not refactorable to last of list, due to imports (see issue #11)"
- (car (reverse (list 1 2 3)))


test: "first reverse of list refactorable to last of list"
------------------------------
(require racket/list)
(first (reverse (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(last (list 1 2 3))
------------------------------


test: "comparison to empty list refactorable to use of null? predicate"
- (eq? (list 1 2 3) '())
- (eq? (list 1 2 3) (list))
- (eq? (list 1 2 3) null)
- (eqv? (list 1 2 3) '())
- (eqv? (list 1 2 3) (list))
- (eqv? (list 1 2 3) null)
- (equal? (list 1 2 3) '())
- (equal? (list 1 2 3) (list))
- (equal? (list 1 2 3) null)
- (null? (list 1 2 3))


test: "(list) refactorable to '()"
- (list)
- '()


test: "null refactorable to '()"
- null
- '()


test: "(apply append (map ...)) refactorable to single-pass append-map"
------------------------------
(require racket/list)
(define (f x) (list x x x))
(apply append (map f (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(define (f x) (list x x x))
(append-map f (list 1 2 3))
------------------------------


test: "(append* (map ...)) refactorable to single-pass append-map"
------------------------------
(require racket/list)
(define (f x) (list x x x))
(append* (map f (list 1 2 3)))
------------------------------
------------------------------
(require racket/list)
(define (f x) (list x x x))
(append-map f (list 1 2 3))
------------------------------


test: "single-list append removable"
- (append (list 1 2 3))
- (list 1 2 3)


test: "sort by comparator using key refactorable to sort by key"
------------------------------
(define (f x) 42)
(sort (list 1 2 3) (λ (a b) (< (f a) (f b))))
------------------------------
------------------------------
(define (f x) 42)
(sort (list 1 2 3) < #:key f)
------------------------------
