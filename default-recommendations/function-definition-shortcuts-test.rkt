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


test: "lambda variable definition with no arguments to function definition"
------------------------------
#lang racket/base
(define f
  (λ ()
    1))
------------------------------
#lang racket/base
(define (f)
  1)
------------------------------


test: "one-line lambda variable definition to one-line function definition"
------------------------------
#lang racket/base
(define f (λ (a b c) 1))
------------------------------
#lang racket/base
(define (f a b c) 1)
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


test: "nested lambda variable definition to function definition"
------------------------------
#lang racket/base
(define f
  (λ (a)
    (λ (b)
      1)))
------------------------------
#lang racket/base
(define ((f a) b)
  1)
------------------------------


test: "nested lambda function definition to function definition"
------------------------------
#lang racket/base
(define (f a)
  (λ (b)
    (λ (c)
      1)))
------------------------------
#lang racket/base
(define (((f a) b) c)
  1)
------------------------------


test: "case-lambda with default arg"
------------------------------
#lang racket/base
(define f
  (case-lambda
    [()
     (f 1)]
    [(x)
     1]))
------------------------------
#lang racket/base
(define (f [x 1])
  1)
------------------------------


test: "case-lambda with default arg and required args"
------------------------------
#lang racket/base
(define f
  (case-lambda
    [(a b c)
     (f a b c 1)]
    [(a b c x)
     1]))
------------------------------
#lang racket/base
(define (f a b c [x 1])
  1)
------------------------------


test: "case-lambda with default arg not refactorable when required args out of order"
------------------------------
#lang racket/base
(define f
  (case-lambda
    [(a b c)
     (f c b a 1)]
    [(a b c x)
     1]))
------------------------------


test: "case-lambda with default arg and multiple body forms"
------------------------------
#lang racket/base
(define f
  (case-lambda
    [()
     (f 1)]
    [(x)
     (void)
     1]))
------------------------------
#lang racket/base
(define (f [x 1])
  (void)
  1)
------------------------------


test: "case-lambda with default arg and body form with interior comments"
------------------------------
#lang racket/base
(define f
  (case-lambda
    [()
     (f 1)]
    [(x)
     (begin
       ;;comment
       1)]))
------------------------------
#lang racket/base
(define (f [x 1])
  (begin
    ;;comment
    1))
------------------------------
