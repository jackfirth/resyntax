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


test: "let binding with commented body refactorable"
------------------------------
(define (f)
  (let ([x 1])
    (void)
    ;; Comment
    1))
------------------------------
------------------------------
(define (f)
  (define x 1)
  (void)
  ;; Comment
  1)
------------------------------


test: "let binding with comments before let form not refactorable (yet)"
------------------------------
(define (f)
  ;; Comment
  (void)
  ;; Comment
  (let ([x 1])
    1))
------------------------------
