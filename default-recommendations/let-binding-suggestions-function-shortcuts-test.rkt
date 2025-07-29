#lang resyntax/test

require:
resyntax/default-recommendations
let-binding-suggestions

header:
-
#lang racket/base

test:
"let binding to lambda"
------------------------------
(define (f)
  (let ([g (λ (x y) 1)]) g))
==============================
(define (f)
  (define (g x y)
    1)
  g)
------------------------------

test:
"let binding to lambda with keyword args"
------------------------------
(define (f)
  (let ([g (λ (#:x x #:y y) 1)]) g))
==============================
(define (f)
  (define (g #:x x #:y y)
    1)
  g)
------------------------------

test:
"let binding to lambda with optional args"
------------------------------
(define (f)
  (let ([g (λ ([x 1] [y 1]) 1)]) g))
==============================
(define (f)
  (define (g [x 1] [y 1])
    1)
  g)
------------------------------

test:
"let binding to lambda with only rest args"
------------------------------
(define (f)
  (let ([g (λ xs 1)]) g))
==============================
(define (f)
  (define (g . xs)
    1)
  g)
------------------------------

test:
"let binding to lambda with positional and rest args"
------------------------------
(define (f)
  (let ([g (λ (x y . zs) 1)]) g))
==============================
(define (f)
  (define (g x y . zs)
    1)
  g)
------------------------------
