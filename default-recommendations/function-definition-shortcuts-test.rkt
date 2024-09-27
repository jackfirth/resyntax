#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations function-definition-shortcuts


header:
- #lang racket/base


test: "lambda variable definition to function definition"
------------------------------
(define f
  (λ (a b c)
    1))
------------------------------
------------------------------
(define (f a b c)
  1)
------------------------------


test: "lambda variable definition with no arguments to function definition"
------------------------------
(define f
  (λ ()
    1))
------------------------------
------------------------------
(define (f)
  1)
------------------------------


test: "one-line lambda variable definition to function definition"
------------------------------
(define f (λ (a b c) 1))
------------------------------
------------------------------
(define (f a b c)
  1)
------------------------------


test: "lambda variable definition with only rest argument to function definition"
------------------------------
(define f
  (λ xs
    1))
------------------------------
------------------------------
(define (f . xs)
  1)
------------------------------


test: "lambda variable definition with rest argument to function definition"
------------------------------
(define f
  (λ (a b c . xs)
    1))
------------------------------
------------------------------
(define (f a b c . xs)
  1)
------------------------------


test: "class lambda variable definition not refactorable"
------------------------------
(require racket/class)
(class object%
  (define f
    (λ (a b c)
      1)))
------------------------------


test: "lambda function definition to function definition"
------------------------------
(define (f a b c)
  (λ (x y z)
    1))
------------------------------
------------------------------
(define ((f a b c) x y z)
  1)
------------------------------


test: "lambda function definition with closed-over expressions not refactorable"
------------------------------
(define (f a b c)
  (displayln a)
  (λ (x y z)
    1))
------------------------------


test: "thunk function definition not refactorable (it's too terse)"
------------------------------
(define (f a b c)
  (λ ()
    1))
------------------------------


test: "lambda variable definition with commented first body not refactorable (yet)"
------------------------------
(define f
  (λ (a)
    ; comment
    (void)
    1))
------------------------------


test: "lambda variable definition with commented second body refactorable to definition"
------------------------------
(define f
  (λ (a)
    (void)
    ; comment
    1))
------------------------------
------------------------------
(define (f a)
  (void)
  ; comment
  1)
------------------------------


test: "nested lambda variable definition to function definition"
------------------------------
(define f
  (λ (a)
    (λ (b)
      1)))
------------------------------
------------------------------
(define ((f a) b)
  1)
------------------------------


test: "nested lambda variable definition with thunk to function definition returning thunk"
------------------------------
(define f
  (λ (a)
    (λ () 1)))
------------------------------
------------------------------
(define (f a)
  (λ () 1))
------------------------------


test: "nested lambda function definition to function definition"
------------------------------
(define (f a)
  (λ (b)
    (λ (c)
      1)))
------------------------------
------------------------------
(define (((f a) b) c)
  1)
------------------------------


test: "nested lambda variable with multiple multiline headers not refactorable"
------------------------------
(define f
  (λ (a
      b
      c)
    (λ (x
        y
        z)
      1)))
------------------------------


test: "function with multiline header returning lambda not refactorable"
------------------------------
(define (f a
           b
           c)
  (λ (x)
    1))
------------------------------


test: "function returning lambda with multiline header not refactorable"
------------------------------
(define (f a)
  (λ (x
      y
      z)
    1))
------------------------------


test: "case-lambda with default arg"
------------------------------
(define f
  (case-lambda
    [()
     (f 1)]
    [(x)
     1]))
------------------------------
------------------------------
(define (f [x 1])
  1)
------------------------------


test: "case-lambda with default arg and required args"
------------------------------
(define f
  (case-lambda
    [(a b c)
     (f a b c 1)]
    [(a b c x)
     1]))
------------------------------
------------------------------
(define (f a b c [x 1])
  1)
------------------------------


test: "case-lambda with default arg not refactorable when required args out of order"
------------------------------
(define f
  (case-lambda
    [(a b c)
     (f c b a 1)]
    [(a b c x)
     1]))
------------------------------


test: "case-lambda with default arg not refactorable when default is a multiline expression"
------------------------------
(define f
  (case-lambda
    [(a b c)
     (f a
        b
        c
        (let ([x 42])
          (displayln x)
          x))]
    [(a b c x)
     1]))
------------------------------


test: "case-lambda with default arg and multiple body forms"
------------------------------
(define f
  (case-lambda
    [()
     (f 1)]
    [(x)
     (void)
     1]))
------------------------------
------------------------------
(define (f [x 1])
  (void)
  1)
------------------------------


test: "case-lambda with default arg and body form with interior comments"
------------------------------
(define f
  (case-lambda
    [()
     (f 1)]
    [(x)
     (begin
       ;;comment
       1)]))
------------------------------
------------------------------
(define (f [x 1])
  (begin
    ;;comment
    1))
------------------------------
