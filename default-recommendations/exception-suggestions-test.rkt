#lang resyntax/test


require: resyntax/default-recommendations exception-suggestions


header:
- #lang racket/base


test: "with-handlers with constant handler refactorable to lambda"
--------------------
(define (f)
  (with-handlers ([exn:fail? #f])
    (void)))
====================
(define (f)
  (with-handlers ([exn:fail? (λ (_) #f)])
    (void)))
--------------------


test: "with-handlers with number handler refactorable to lambda"
--------------------
(with-handlers ([exn:fail? 42])
  (void))
====================
(with-handlers ([exn:fail? (λ (_) 42)])
  (void))
--------------------


test: "with-handlers with string handler refactorable to lambda"
--------------------
(with-handlers ([exn:fail? "error"])
  (void))
====================
(with-handlers ([exn:fail? (λ (_) "error")])
  (void))
--------------------


test: "with-handlers with quoted literal handler refactorable to lambda"
--------------------
(with-handlers ([exn:fail? 'symbol])
  (void))
====================
(with-handlers ([exn:fail? (λ (_) 'symbol)])
  (void))
--------------------


test: "with-handlers with multiple clauses, first has literal"
--------------------
(with-handlers ([exn:fail? #f]
                [exn:break? (λ (e) e)])
  (void))
====================
(with-handlers ([exn:fail? (λ (_) #f)]
                [exn:break? (λ (e) e)])
  (void))
--------------------


test: "with-handlers with multiple clauses, second has literal"
--------------------
(with-handlers ([exn:fail? (λ (e) e)]
                [exn:break? #f])
  (void))
====================
(with-handlers ([exn:fail? (λ (e) e)]
                [exn:break? (λ (_) #f)])
  (void))
--------------------


no-change-test: "with-handlers with procedure handler not refactorable"
--------------------
(with-handlers ([exn:fail? (λ (e) #f)])
  (void))
--------------------


no-change-test: "with-handlers with identifier handler not refactorable"
--------------------
(define (handler e) (displayln e))
(with-handlers ([exn:fail? handler])
  (void))
--------------------


test: "error with format string and arguments refactorable to raise-arguments-error"
--------------------
(define (foo low high)
  (unless (<= low high)
    (error 'foo "low should be less than high, ~a ~a" low high))
  (void))
====================
(define (foo low high)
  (unless (<= low high)
    (raise-arguments-error 'foo "low should be less than high" "low" low "high" high))
  (void))
--------------------


test: "error with single argument refactorable to raise-arguments-error"
--------------------
(define (bar x)
  (when (negative? x)
    (error 'bar "x must be non-negative: ~a" x))
  (void))
====================
(define (bar x)
  (when (negative? x)
    (raise-arguments-error 'bar "x must be non-negative" "x" x))
  (void))
--------------------


no-change-test: "error without format placeholders not refactorable"
--------------------
(define (baz)
  (error 'baz "something went wrong"))
--------------------


no-change-test: "error with non-identifier arguments not refactorable"
--------------------
(define (qux x)
  (error 'qux "value is: ~a" (+ x 1)))
--------------------


no-change-test: "error with mismatched placeholder count not refactorable"
--------------------
(define (mismatch x y)
  (error 'mismatch "values: ~a ~a ~a" x y))
--------------------


no-change-test: "error with more arguments than placeholders not refactorable"
--------------------
(define (extra x y)
  (error 'extra "value: ~a" x y))
--------------------
