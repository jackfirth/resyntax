#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations let-binding-suggestions


header:
- #lang racket/base


test: "single let binding"
------------------------------
(define (f)
  (let ([x 1])
    1))
------------------------------
------------------------------
(define (f)
  (define x 1)
  1)
------------------------------


test: "single let* binding"
------------------------------
(define (f)
  (let* ([x 1])
    1))
------------------------------
------------------------------
(define (f)
  (define x 1)
  1)
------------------------------


test: "single-clause let-values binding"
------------------------------
(define (f)
  (let-values ([(x y) (values 1 1)])
    1))
------------------------------
------------------------------
(define (f)
  (define-values (x y) (values 1 1))
  1)
------------------------------


test: "single-clause let*-values binding"
------------------------------
(define (f)
  (let*-values ([(x y) (values 1 1)])
    1))
------------------------------
------------------------------
(define (f)
  (define-values (x y) (values 1 1))
  1)
------------------------------


test: "multiple let bindings"
------------------------------
(define (f)
  (let ([x 1]
        [y 1])
    1))
------------------------------
------------------------------
(define (f)
  (define x 1)
  (define y 1)
  1)
------------------------------


test: "multiple let* bindings"
------------------------------
(define (f)
  (let* ([x 1]
         [y 1])
    1))
------------------------------
------------------------------
(define (f)
  (define x 1)
  (define y 1)
  1)
------------------------------


test: "multiple let-values bindings"
------------------------------
(define (f)
  (let-values ([(x y) (values 1 1)]
               [(a b) (values 1 1)])
    1))
------------------------------
------------------------------
(define (f)
  (define-values (x y) (values 1 1))
  (define-values (a b) (values 1 1))
  1)
------------------------------


test: "multiple let*-values bindings"
------------------------------
(define (f)
  (let*-values ([(x y) (values 1 1)]
                [(a b) (values 1 1)])
    1))
------------------------------
------------------------------
(define (f)
  (define-values (x y) (values 1 1))
  (define-values (a b) (values 1 1))
  1)
------------------------------


test: "self-shadowing let binding isn't refactorable"
------------------------------
(define (f x)
  (let ([x x])
    1))
------------------------------


test: "self-shadowing let* binding isn't refactorable"
------------------------------
(define (f x)
  (let* ([x x])
    1))
------------------------------


test: "self-shadowing let-values binding clause isn't refactorable"
------------------------------
(define (f x)
  (let-values ([(x y) (values x 1)])
    1))
------------------------------


test: "self-shadowing let*-values binding clause isn't refactorable"
------------------------------
(define (f x)
  (let*-values ([(x y) (values x 1)])
    1))
------------------------------


test: "let forms inside lambdas"
------------------------------
(λ ()
  (let ([x 1])
    1))
------------------------------
------------------------------
(λ ()
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let forms"
------------------------------
(define a 1)
(let ([a a])
  (let ([x 1])
    1))
------------------------------
------------------------------
(define a 1)
(let ([a a])
  (define x 1)
  1)
------------------------------


test: "let forms inside let loops"
------------------------------
(let loop ()
  (let ([x 1])
    1))
------------------------------
------------------------------
(let loop ()
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let* forms"
------------------------------
(define a 1)
(let* ([a a]
       [a a])
  (let ([x 1])
    1))
------------------------------
------------------------------
(define a 1)
(let* ([a a]
       [a a])
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let-values forms"
------------------------------
(define a 1)
(let-values ([(a b) (values a 1)])
  (let ([x 1])
    1))
------------------------------
------------------------------
(define a 1)
(let-values ([(a b) (values a 1)])
  (define x 1)
  1)
------------------------------


test: "let forms inside unrefactorable let*-values forms"
------------------------------
(define a 1)
(let*-values ([(a b) (values a 1)])
  (let ([x 1])
    1))
------------------------------
------------------------------
(define a 1)
(let*-values ([(a b) (values a 1)])
  (define x 1)
  1)
------------------------------


test: "let forms inside when forms"
------------------------------
(when #true
  (let ([x 1])
    1))
------------------------------
------------------------------
(when #true
  (define x 1)
  1)
------------------------------


test: "let forms inside unless forms"
------------------------------
(unless #false
  (let ([x 1])
    1))
------------------------------
------------------------------
(unless #false
  (define x 1)
  1)
------------------------------


test: "let forms inside with-handlers forms"
------------------------------
(with-handlers ([exn:fail? void])
  (let ([x 1])
    1))
------------------------------
------------------------------
(with-handlers ([exn:fail? void])
  (define x 1)
  1)
------------------------------


test: "let forms inside parameterize forms"
------------------------------
(define p (make-parameter #false))
(parameterize ([p #true])
  (let ([x 1])
    1))
------------------------------
------------------------------
(define p (make-parameter #false))
(parameterize ([p #true])
  (define x 1)
  1)
------------------------------


test: "let forms inside for loop bodies"
------------------------------
(for ([i (in-range 0 10)])
  (let ([x 1])
    (void)))
------------------------------
------------------------------
(for ([i (in-range 0 10)])
  (define x 1)
  (void))
------------------------------


test: "named lets which don't refer to the name are refactorable to unnamed lets"
------------------------------
(let loop ([x 1])
  x)
------------------------------
------------------------------
(let ([x 1])
  x)
------------------------------


test: "named lets which do refer to the name aren't refactorable to unnamed lets"
------------------------------
(let loop ([x 1])
  (if (zero? x)
      x
      (loop (sub1 x))))
------------------------------


test: "let-values expressions with an immediate call are refactorable to call-with-values"
- (let-values ([(x y z) (values 1 2 3)]) (list x y z))
- (call-with-values (λ () (values 1 2 3)) list)


test: "let-values expressions with an immediate call with different order aren't refactorable"
- (let-values ([(x y z) (values 1 2 3)]) (list z y x))
