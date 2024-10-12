#lang resyntax/test


require: resyntax/default-recommendations definition-shortcuts


header:
- #lang racket/base


test: "define-values with values refactorable to separate definitions"
------------------------------
(define (foo)
  (define-values (a b c) (values 1 2 3))
  (+ a b c))
------------------------------
------------------------------
(define (foo)
  (define a 1)
  (define b 2)
  (define c 3)
  (+ a b c))
------------------------------


test: "define-values with values and body before refactorable to separate definitions"
------------------------------
(define (foo)
  (displayln "foo")
  (define-values (a b c) (values 1 2 3))
  (+ a b c))
------------------------------
------------------------------
(define (foo)
  (displayln "foo")
  (define a 1)
  (define b 2)
  (define c 3)
  (+ a b c))
------------------------------


test: "define-values with values inside cond refactorable to separate definitions"
------------------------------
(define (foo condition)
  (cond
    [condition
     (define-values (a b c) (values 1 2 3))
     (+ a b c)]
    [else (displayln "else")]))
------------------------------
------------------------------
(define (foo condition)
  (cond
    [condition
     (define a 1)
     (define b 2)
     (define c 3)
     (+ a b c)]
    [else (displayln "else")]))
------------------------------


test: "refactoring define-values to separate definitions doesn't reformat context"
------------------------------
(define (foo)

  (  displayln   "foo"   )

  (define-values (a b c) (values 1 2 3))

  (  +  a  b   c ))
------------------------------
------------------------------
(define (foo)

  (  displayln   "foo"   )

  (define a 1)
  (define b 2)
  (define c 3)

  (  +  a  b   c ))
------------------------------


test: "immediately returned variable definition can be inlined"
------------------------------
(define (foo)
  (define x 1)
  x)
------------------------------
------------------------------
(define (foo)
  1)
------------------------------


test: "immediately returned function definition cannot be inlined"
------------------------------
(define (foo)
  (define (x)
    1)
  x)
------------------------------


test: "immediately used variable definition cannot be inlined"
------------------------------
(define (foo)
  (define x 1)
  (* x 2))
------------------------------


test: "inlining immediately returned variable definition doesn't reformat entire context"
------------------------------
(define (foo)

  ( displayln    "foo" )

  (define x 1)
  x)
------------------------------
------------------------------
(define (foo)

  ( displayln    "foo" )

  1)
------------------------------


test: "inlining immediately returned variable definition in empty context does reformat"
------------------------------
(map (λ (x)
       (define y (* x 2))
       y)
     (list 1 2 3))
------------------------------
------------------------------
(map (λ (x) (* x 2))
     (list 1 2 3))
------------------------------
