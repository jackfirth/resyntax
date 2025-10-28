#lang resyntax/test


require: resyntax/default-recommendations match-let-replacement


header:
------------------------------
#lang racket/base
(require racket/match)
------------------------------


test: "single-binding match-let expressions can be replaced with match-define"
------------------------------
(define (foo x)
  (displayln "foo?")
  (match-let ([(list a b c) x])
    (displayln "foo!")
    (+ a b c)))
==============================
(define (foo x)
  (displayln "foo?")
  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "single-binding match-let* expressions can be replaced with match-define"
------------------------------
(define (foo x)
  (displayln "foo?")
  (match-let* ([(list a b c) x])
    (displayln "foo!")
    (+ a b c)))
==============================
(define (foo x)
  (displayln "foo?")
  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "single-binding match-letrec expressions can be replaced with match-define"
------------------------------
(define (foo x)
  (displayln "foo?")
  (match-letrec ([(list a b c) x])
    (displayln "foo!")
    (+ a b c)))
==============================
(define (foo x)
  (displayln "foo?")
  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "migrating single-binding match-let expressions to match-define doesn't reformat context"
------------------------------
(define (foo x)

  (  displayln "foo?"   )

  (match-let ([(list a b c) x])
    (displayln "foo!")
    (+ a b c)))
==============================
(define (foo x)

  (  displayln "foo?"   )

  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "migrating single-binding match-let expressions in single-form contexts does reformat"
------------------------------
(map (λ (x) (match-let ([(list a b c) x]) (+ a b c)))
     (list (list 1 2 3) (list 4 5 6)))
==============================
(map (λ (x)
       (match-define (list a b c) x)
       (+ a b c))
     (list (list 1 2 3) (list 4 5 6)))
------------------------------


test: "single-binding match-let expressions inside cond can be replaced with match-define"
------------------------------
(define (foo x condition)
  (cond
    [condition
     (displayln "foo?")
     (match-let ([(list a b c) x])
       (displayln "foo!")
       (+ a b c))]
    [else (displayln "else")]))
==============================
(define (foo x condition)
  (cond
    [condition
     (displayln "foo?")
     (match-define (list a b c) x)
     (displayln "foo!")
     (+ a b c)]
    [else (displayln "else")]))
------------------------------


no-change-test:
"single-binding match-let not migratable when pattern bindings conflict with surrounding context"
------------------------------
(define (foo x)
  (define a 42)
  (match-let ([(list a b c) x])
    a))
------------------------------


no-change-test:
"single-binding match-let not migratable when pattern would bind subject expression"
------------------------------
(define (foo x)
  (match-let ([(list x y z) x])
    x))
------------------------------


no-change-test: "multiple-binding match-let should not be converted to match-define"
------------------------------
(define (foo x y)
  (match-let ([(list a b) x]
              [(list c d) y])
    (+ a b c d)))
------------------------------


no-change-test: "multiple-binding match-let* should not be converted to match-define"
------------------------------
(define (foo x)
  (match-let* ([(list a b) x]
               [(list c d) (list a b)])
    (+ a b c d)))
------------------------------
