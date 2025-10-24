#lang resyntax/test


require: resyntax/default-recommendations let-binding-suggestions


header:
- #lang racket/base


test: "define-let-to-multi-define with single binding"
------------------------------
(define (f)
  (define a (let ([b 1]) (+ b 10)))
  a)
==============================
(define (f)
  (define b 1)
  (define a (+ b 10))
  a)
------------------------------


test: "define-let-to-multi-define with two bindings"
------------------------------
(define (f)
  (define a (let ([b 1] [c 2]) (+ b c 10)))
  a)
==============================
(define (f)
  (define b 1)
  (define c 2)
  (define a (+ b c 10))
  a)
------------------------------


test: "define-let-to-multi-define with three bindings"
------------------------------
(define (f)
  (define result (let ([x 1] [y 2] [z 3]) (* x y z)))
  result)
==============================
(define (f)
  (define x 1)
  (define y 2)
  (define z 3)
  (define result (* x y z))
  result)
------------------------------


test: "define-let-to-multi-define with body-before"
------------------------------
(define (f)
  (displayln "foo")
  (define a (let ([b 1] [c 2]) (+ b c)))
  a)
==============================
(define (f)
  (displayln "foo")
  (define b 1)
  (define c 2)
  (define a (+ b c))
  a)
------------------------------


test: "define-let-to-multi-define with body-after"
------------------------------
(define (f)
  (define a (let ([b 1] [c 2]) (+ b c)))
  (displayln "bar")
  a)
==============================
(define (f)
  (define b 1)
  (define c 2)
  (define a (+ b c))
  (displayln "bar")
  a)
------------------------------


test: "define-let-to-multi-define preserves complex expressions"
------------------------------
(define (f x)
  (define result
    (let ([sum (+ x 1)]
          [product (* x 2)])
      (+ sum product)))
  result)
==============================
(define (f x)
  (define sum (+ x 1))
  (define product (* x 2))
  (define result (+ sum product))
  result)
------------------------------


no-change-test: "define-let-to-multi-define doesn't apply when bindings shadow outer scope"
------------------------------
(define (f b)
  (define a (let ([b 1]) (+ b 10)))
  a)
------------------------------


no-change-test: "define-let-to-multi-define doesn't apply when later binding depends on earlier"
------------------------------
(define (f)
  (define a (let ([b 1] [c b]) (+ b c)))
  a)
------------------------------
