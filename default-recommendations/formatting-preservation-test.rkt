#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations default-recommendations


header:
- #lang racket/base


test: "refactoring an expression doesn't affect formatting of unrefactored code"
----------------------------------------
( displayln "foo" )
(or 1 (or 2 3))
( displayln "bar" )
----------------------------------------
----------------------------------------
( displayln "foo" )
(or 1 2 3)
( displayln "bar" )
----------------------------------------


test: "define-let-to-double-define doesn't reformat the entire definition context"
----------------------------------------
(define (f)
  ( displayln "foo" )
  (define y (let ([x 1]) (* x 2)))
  ( displayln "bar" ))
----------------------------------------
----------------------------------------
(define (f)
  ( displayln "foo" )
  (define x 1)
  (define y (* x 2))
  ( displayln "bar" ))
----------------------------------------


test: "let-to-define doesn't reformat the entire definition context"
----------------------------------------
(define (f)
  ( displayln "foo" )
  (let ([x 1])
    (* x 2)))
----------------------------------------
----------------------------------------
(define (f)
  ( displayln "foo" )
  (define x 1)
  (* x 2))
----------------------------------------


test: "cond-let-to-cond-define doesn't reformat the entire cond expression"
----------------------------------------
(define (f c1 c2)
  (cond
    [c1 ( displayln "foo" )]
    [c2
     ( displayln "bar" )
     (let ([x 1])
       (* x 2))]
    [else ( displayln "else" )]))
----------------------------------------
----------------------------------------
(define (f c1 c2)
  (cond
    [c1 ( displayln "foo" )]
    [c2
     ( displayln "bar" )
     (define x 1)
     (* x 2)]
    [else ( displayln "else" )]))
----------------------------------------
