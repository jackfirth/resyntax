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
