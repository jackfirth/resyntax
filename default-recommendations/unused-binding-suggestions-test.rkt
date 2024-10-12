#lang resyntax/test


require: resyntax/default-recommendations unused-binding-suggestions


header:
- #lang racket/base


test: "should remove unused function definitions from internal definition contexts"
------------------------------
(define (foo)
  (define (bar)
    (displayln "bar"))
  42)
------------------------------
------------------------------
(define (foo)
  42)
------------------------------


test: "should not remove used function definitions from internal definition contexts"
------------------------------
(define (foo)
  (define (bar)
    (displayln "bar"))
  (bar)
  42)
------------------------------


test: "removing unused function definitions shouldn't reformat entire context"
------------------------------
(define (foo)
  ( displayln "foo" )

  
  (define (bar)
    (displayln "bar"))

  (define x 2)
  ( * x 2 ))
------------------------------
------------------------------
(define (foo)
  ( displayln "foo" )

  
  (define x 2)
  ( * x 2 ))
------------------------------


test: "should remove unused side-effect-free variable definitions from internal definition contexts"
------------------------------
(define (foo)
  (define bar "bar")
  42)
------------------------------
------------------------------
(define (foo)
  42)
------------------------------
