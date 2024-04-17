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


test: "filter with andmap and equal? to intersect lists refactorable to remove*"
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(filter (λ (id)
          (andmap (λ (id2) (not (equal? id id2)))
                  old-ids))
        new-ids)
------------------------------
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(remove* old-ids new-ids)
------------------------------


test: "filter with andmap and eqv? to intersect lists refactorable to remv*"
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(filter (λ (id)
          (andmap (λ (id2) (not (eqv? id id2)))
                  old-ids))
        new-ids)
------------------------------
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(remv* old-ids new-ids)
------------------------------


test: "filter with andmap and eq? to intersect lists refactorable to remq*"
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(filter (λ (id)
          (andmap (λ (id2) (not (eq? id id2)))
                  old-ids))
        new-ids)
------------------------------
------------------------------
(define old-ids '(a b c))
(define new-ids '(b c d e))
(remq* old-ids new-ids)
------------------------------


test: "sort by comparator using key refactorable to sort by key"
------------------------------
(define (f x) 42)
(sort (list 1 2 3) (λ (a b) (< (f a) (f b))))
------------------------------
------------------------------
(define (f x) 42)
(sort (list 1 2 3) < #:key f)
------------------------------


test: "unnecessary quasiquotation refactorable to list"
------------------------------
(define (f x y z)
  `(,x ,y ,z))
------------------------------
------------------------------
(define (f x y z)
  (list x y z))
------------------------------


test: "unnecessary quasiquotation with constants refactorable to list"
------------------------------
(define (f x y z)
  `(,x 1 ,y 2 ,z 3))
------------------------------
------------------------------
(define (f x y z)
  (list x 1 y 2 z 3))
------------------------------
