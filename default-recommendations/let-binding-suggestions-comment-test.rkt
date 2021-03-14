#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations let-binding-suggestions


header:
- #lang racket/base


test: "let binding with commented right-hand-side expression"
------------------------------
(define (f)
  (let ([x
         ;; The number one
         1])
    1))
------------------------------
------------------------------
(define (f)
  (define x
    ;; The number one
    1)
  1)
------------------------------
