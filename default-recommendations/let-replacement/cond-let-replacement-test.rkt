#lang resyntax/test


require: resyntax/default-recommendations cond-let-replacement


header:
- #lang racket/base


test: "cond with nested let refactorable to cond with define"
------------------------------
(define (f a c)
  (cond
    [a
     (let ([x "stuff"])
       x)]
    [else c]))
==============================
(define (f a c)
  (cond
    [a
     (define x "stuff")
     x]
    [else c]))
------------------------------


test: "cond with nested let in else clause refactorable to cond with define"
------------------------------
(define (f a b)
  (cond
    [a b]
    [else
     (let ([x "stuff"])
       x)]))
==============================
(define (f a b)
  (cond
    [a b]
    [else
     (define x "stuff")
     x]))
------------------------------
