#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations let-binding-suggestions


test: "single let binding"
------------------------------
#lang racket/base
(define (f)
  (let ([x 1])
    1))
------------------------------
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
------------------------------
#lang racket/base
(define (f)
  (define-values (x y) (values 1 1))
  1)
------------------------------


test: "single-clause let*-values binding"
------------------------------
#lang racket/base
(define (f)
  (let*-values ([(x y) (values 1 1)])
    1))
------------------------------
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
------------------------------
#lang racket/base
(define (f)
  (define-values (x y) (values 1 1))
  (define-values (a b) (values 1 1))
  1)
------------------------------


test: "multiple let*-values bindings"
------------------------------
#lang racket/base
(define (f)
  (let*-values ([(x y) (values 1 1)]
                [(a b) (values 1 1)])
    1))
------------------------------
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


test: "self-shadowing let*-values binding clause isn't refactorable"
------------------------------
#lang racket/base
(define (f x)
  (let*-values ([(x y) (values x 1)])
    1))
------------------------------


test: "let forms inside lambdas"
------------------------------
#lang racket/base
(λ ()
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(λ ()
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let forms"
------------------------------
#lang racket/base
(define a 1)
(let ([a a])
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(define a 1)
(let ([a a])
  (define x 1)
  1)
------------------------------


test: "let forms inside let loops"
------------------------------
#lang racket/base
(let loop ()
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(let loop ()
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let* forms"
------------------------------
#lang racket/base
(define a 1)
(let* ([a a]
       [a a])
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(define a 1)
(let* ([a a]
       [a a])
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let-values forms"
------------------------------
#lang racket/base
(define a 1)
(let-values ([(a b) (values a 1)])
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(define a 1)
(let-values ([(a b) (values a 1)])
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let*-values forms"
------------------------------
#lang racket/base
(define a 1)
(let*-values ([(a b) (values a 1)])
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(define a 1)
(let*-values ([(a b) (values a 1)])
  (define x 1)
  1)
------------------------------


test: "let forms inside when forms"
------------------------------
#lang racket/base
(when #true
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(when #true
  (define x 1)
  1)
------------------------------


test: "let forms inside unless forms"
------------------------------
#lang racket/base
(unless #false
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(unless #false
  (define x 1)
  1)
------------------------------


test: "let forms inside with-handlers forms"
------------------------------
#lang racket/base
(with-handlers ([exn:fail? void])
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(with-handlers ([exn:fail? void])
  (define x 1)
  1)
------------------------------


test: "let forms inside parameterize forms"
------------------------------
#lang racket/base
(define p (make-parameter #false))
(parameterize ([p #true])
  (let ([x 1])
    1))
------------------------------
------------------------------
#lang racket/base
(define p (make-parameter #false))
(parameterize ([p #true])
  (define x 1)
  1)
------------------------------


test: "let forms inside for loop bodies"
------------------------------
#lang racket/base
(for ([i (in-range 0 10)])
  (let ([x 1])
    (void)))
------------------------------
------------------------------
#lang racket/base
(for ([i (in-range 0 10)])
  (define x 1)
  (void))
------------------------------


test: "named lets which don't refer to the name are refactorable to unnamed lets"
------------------------------
#lang racket/base
(let loop ([x 1])
  x)
------------------------------
------------------------------
#lang racket/base
(let ([x 1])
  x)
------------------------------


test: "named lets which do refer to the name aren't refactorable to unnamed lets"
------------------------------
#lang racket/base
(let loop ([x 1])
  (if (zero? x)
      x
      (loop (sub1 x))))
------------------------------
