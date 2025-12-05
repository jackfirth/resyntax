#lang resyntax/test


require: resyntax/default-recommendations/loops/fuse-map-with-for fuse-map-with-for


header:
- #lang racket/base


test: "map producing list for for* loop can be fused"
--------------------
(define (f xs g h)
  (define ys (map (λ (x) (g x)) xs))
  (for* ([y (in-list ys)]
         [z (in-list (h y))])
    (displayln z)))
====================
(define (f xs g h)
  (for* ([x (in-list xs)]
         [y (in-list (g x))]
         [z (in-list (h y))])
    (displayln z)))
--------------------


test: "map producing list for for loop can be fused"
--------------------
(define (f xs g)
  (define ys (map (λ (x) (g x)) xs))
  (for ([y (in-list ys)])
    (displayln y)))
====================
(define (f xs g)
  (for ([x (in-list xs)])
    (define y (g x))
    (displayln y)))
--------------------


no-change-test: "map with short lambda but ys used elsewhere not refactorable"
--------------------
(define (f xs g h)
  (define ys (map (λ (x) (g x)) xs))
  (for* ([y (in-list ys)]
         [z (in-list (h y))])
    (displayln z))
  (displayln ys))
--------------------


test: "map with lambda that has multiple body forms is refactorable"
--------------------
(define (f xs g)
  (define ys (map (λ (x) (displayln x) (g x)) xs))
  (for ([y (in-list ys)])
    (displayln y)))
====================
(define (f xs g)
  (for ([x (in-list xs)])
    (displayln x)
    (define y (g x))
    (displayln y)))
--------------------


test: "map with long single-body lambda is refactorable"
--------------------
(define (f xs)
  (define long-name 42)
  (define ys
    (map (λ (x)
           (+ x long-name))
         xs))
  (for ([y (in-list ys)])
    (displayln y)))
====================
(define (f xs)
  (define long-name 42)
  (for ([x (in-list xs)])
    (define y (+ x long-name))
    (displayln y)))
--------------------
