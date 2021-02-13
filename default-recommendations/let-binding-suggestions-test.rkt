#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations/let-binding-suggestions let-binding-suggestions


test: "single let binding"
------------------------------
#lang racket/base
(define (f)
  (let ([x 1])
    1))
------------------------------
#lang racket/base
(define (f)
  (define x 1)
  1)
------------------------------


test: "single let* binding"
------------------------------
#lang racket/base
(define (f)
  (let* ([x 1])
    1))
------------------------------
#lang racket/base
(define (f)
  (define x 1)
  1)
------------------------------


test: "single-clause let-values binding"
------------------------------
#lang racket/base
(define (f)
  (let-values ([(x y) (values 1 1)])
    1))
------------------------------
#lang racket/base
(define (f)
  (define-values (x y) (values 1 1))
  1)
------------------------------


test: "multiple let bindings"
------------------------------
#lang racket/base
(define (f)
  (let ([x 1]
        [y 1])
    1))
------------------------------
#lang racket/base
(define (f)
  (define x 1)
  (define y 1)
  1)
------------------------------


test: "multiple let* bindings"
------------------------------
#lang racket/base
(define (f)
  (let* ([x 1]
         [y 1])
    1))
------------------------------
#lang racket/base
(define (f)
  (define x 1)
  (define y 1)
  1)
------------------------------


test: "multiple let-values bindings"
------------------------------
#lang racket/base
(define (f)
  (let-values ([(x y) (values 1 1)]
               [(a b) (values 1 1)])
    1))
------------------------------
#lang racket/base
(define (f)
  (define-values (x y) (values 1 1))
  (define-values (a b) (values 1 1))
  1)
------------------------------


test: "self-shadowing let binding isn't refactorable"
------------------------------
#lang racket/base
(define (f x)
  (let ([x x])
    1))
------------------------------


test: "self-shadowing let* binding isn't refactorable"
------------------------------
#lang racket/base
(define (f x)
  (let* ([x x])
    1))
------------------------------


test: "self-shadowing let-values binding clause isn't refactorable"
------------------------------
#lang racket/base
(define (f x)
  (let-values ([(x y) (values x 1)])
    1))
------------------------------
