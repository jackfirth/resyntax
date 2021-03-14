#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations let-binding-suggestions


test: "let binding to lambda"
------------------------------
#lang racket/base
(define (f)
  (let ([g (λ (x y) 1)])
    1))
------------------------------
------------------------------
#lang racket/base
(define (f)
  (define (g x y)
    1)
  1)
------------------------------


test: "let binding to lambda with keyword args"
------------------------------
#lang racket/base
(define (f)
  (let ([g (λ (#:x x #:y y) 1)])
    1))
------------------------------
------------------------------
#lang racket/base
(define (f)
  (define (g #:x x #:y y)
    1)
  1)
------------------------------


test: "let binding to lambda with optional args"
------------------------------
#lang racket/base
(define (f)
  (let ([g (λ ([x 1] [y 1]) 1)])
    1))
------------------------------
------------------------------
#lang racket/base
(define (f)
  (define (g [x 1] [y 1])
    1)
  1)
------------------------------


test: "let binding to lambda with only rest args"
------------------------------
#lang racket/base
(define (f)
  (let ([g (λ xs 1)])
    1))
------------------------------
------------------------------
#lang racket/base
(define (f)
  (define (g . xs)
    1)
  1)
------------------------------


test: "let binding to lambda with positional and rest args"
------------------------------
#lang racket/base
(define (f)
  (let ([g (λ (x y . zs) 1)])
    1))
------------------------------
------------------------------
#lang racket/base
(define (f)
  (define (g x y . zs)
    1)
  1)
------------------------------
