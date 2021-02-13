#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations/let-binding-suggestions let-binding-suggestions


test: "let binding with commented right-hand-side expression"
------------------------------
#lang racket/base
(define (f)
  (let ([x
         ;; The number one
         1])
    1))
------------------------------
#lang racket/base
(define (f)
  (define x
    ;; The number one
    1)
  1)
------------------------------
