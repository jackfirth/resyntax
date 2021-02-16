#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations function-definition-shortcuts


test: "lambda variable definition to function definition"
------------------------------
#lang racket/base
(define f
  (λ (a b c)
    1))
------------------------------
#lang racket/base
(define (f a b c)
  1)
------------------------------


test: "lambda variable definition with only rest argument to function definition"
------------------------------
#lang racket/base
(define f
  (λ xs
    1))
------------------------------
#lang racket/base
(define (f . xs)
  1)
------------------------------


test: "lambda variable definition with rest argument to function definition"
------------------------------
#lang racket/base
(define f
  (λ (a b c . xs)
    1))
------------------------------
#lang racket/base
(define (f a b c . xs)
  1)
------------------------------


test: "lambda function definition to function definition"
------------------------------
#lang racket/base
(define (f a b c)
  (λ (x y z)
    1))
------------------------------
#lang racket/base
(define ((f a b c) x y z)
  1)
------------------------------


test: "lambda function definition with closed-over expressions not refactorable"
------------------------------
#lang racket/base
(define (f a b c)
  (displayln a)
  (λ (x y z)
    1))
------------------------------


test: "lambda variable definition with long header to function definition with preserved formatting"
------------------------------
#lang racket/base
(define f
  (λ (a
      b
      c)
    1))
------------------------------
#lang racket/base
(define (f a
           b
           c)
  1)
------------------------------


test: "lambda variable definition with commented body to definition with preserved comments"
------------------------------
#lang racket/base
(define f
  (λ (a)
    ;; comment before all body forms
    (void)
    ;; comment before last body form
    1))
------------------------------
#lang racket/base
(define (f a)
  ;; comment before all body forms
  (void)
  ;; comment before last body form
  1)
------------------------------
