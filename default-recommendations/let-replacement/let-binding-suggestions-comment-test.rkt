#lang resyntax/test


require: resyntax/default-recommendations let-binding-suggestions


header:
- #lang racket/base


test: "let binding with commented right-hand-side expression"
------------------------------
(define (f)
  (let ([x
         ;; The number one
         1])
    x))
==============================
(define (f)
  (define x
    ;; The number one
    1)
  x)
------------------------------

test: "let binding with commented second clause"
------------------------------
(define (f)
  (let ([x 1]
        ;; The number two
        [y 2])
    (+ x y)))
==============================
(define (f)
  (define x 1)
  ;; The number two
  (define y 2)
  (+ x y))
------------------------------


no-change-test: "let binding with commented first clause not refactorable (yet)"
------------------------------
(define (f)
  (let (;; The number one
        [x 1])
    x))
------------------------------


test: "let binding with commented first body form refactorable"
------------------------------
(define (f)
  (let ([x 1])
    ;; Comment
    (void)
    x))
==============================
(define (f)
  (define x 1)
  ;; Comment
  (void)
  x)
------------------------------


test: "let binding with commented second body form refactorable"
------------------------------
(define (f)
  (let ([x 1])
    (void)
    ;; Comment
    x))
==============================
(define (f)
  (define x 1)
  (void)
  ;; Comment
  x)
------------------------------


test: "let binding with comments before let form refactorable"
------------------------------
(define (f)
  ;; Comment
  (void)
  ;; Comment
  (let ([x 1])
    x))
==============================
(define (f)
  ;; Comment
  (void)
  ;; Comment
  (define x 1)
  x)
------------------------------
