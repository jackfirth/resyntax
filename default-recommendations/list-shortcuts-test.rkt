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


test: "list eq? to quoted empty list refactorable to null? check"
- (eq? (list 1 2 3) '())
- (null? (list 1 2 3))


test: "list eqv? to quoted empty list refactorable to null? check"
- (eqv? (list 1 2 3) '())
- (null? (list 1 2 3))


test: "list equal? to quoted empty list refactorable to null? check"
- (equal? (list 1 2 3) '())
- (null? (list 1 2 3))


test: "list eq? to (list) refactorable to null? check"
- (eq? (list 1 2 3) (list))
- (null? (list 1 2 3))


test: "list eqv? to (list) refactorable to null? check"
- (eqv? (list 1 2 3) (list))
- (null? (list 1 2 3))


test: "list equal? to (list) refactorable to null? check"
- (equal? (list 1 2 3) (list))
- (null? (list 1 2 3))


test: "list eq? to null refactorable to null? check"
- (eq? (list 1 2 3) null)
- (null? (list 1 2 3))


test: "list eqv? to null refactorable to null? check"
- (eqv? (list 1 2 3) null)
- (null? (list 1 2 3))


test: "list equal? to null refactorable to null? check"
- (equal? (list 1 2 3) null)
- (null? (list 1 2 3))


test: "(list) refactorable to '() check"
- (list)
- '()


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
