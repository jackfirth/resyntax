#lang resyntax/test


require: resyntax/default-recommendations match-shortcuts


header:
------------------------------
#lang racket/base
(require racket/match)
------------------------------


test: "single-clause match expressions can be replaced with match-define expressions"
------------------------------
(define (foo x)
  (displayln "foo?")
  (match x
    [(list a b c)
     (displayln "foo!")
     (+ a b c)]))
------------------------------
------------------------------
(define (foo x)
  (displayln "foo?")
  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "migrating single-clause match expressions to match-define doesn't reformat context"
------------------------------
(define (foo x)

  (  displayln "foo?"   )

  (match x
    [(list a b c)
     (displayln "foo!")
     (+ a b c)]))
------------------------------
------------------------------
(define (foo x)

  (  displayln "foo?"   )

  (match-define (list a b c) x)
  (displayln "foo!")
  (+ a b c))
------------------------------


test: "migrating single-clause match expressions in single-form contexts does reformat"
------------------------------
(map (λ (x) (match x [(list a b c) (+ a b c)]))
     (list (list 1 2 3) (list 4 5 6)))
------------------------------
------------------------------
(map (λ (x)
       (match-define (list a b c) x)
       (+ a b c))
     (list (list 1 2 3) (list 4 5 6)))
------------------------------


test: "single-clause match expressions inside cond can be replaced with match-define expressions"
------------------------------
(define (foo x condition)
  (cond
    [condition
     (displayln "foo?")
     (match x
       [(list a b c)
        (displayln "foo!")
        (+ a b c)])]
    [else (displayln "else")]))
------------------------------
------------------------------
(define (foo x condition)
  (cond
    [condition
     (displayln "foo?")
     (match-define (list a b c) x)
     (displayln "foo!")
     (+ a b c)]
    [else (displayln "else")]))
------------------------------


test: "single-clause match not migratable when pattern bindings conflict with surrounding context"
------------------------------
(define (foo x)
  (define a 42)
  (match x
    [(list a b c) a]))
------------------------------


test: "single-clause match not migratable when pattern would bind subject expression"
------------------------------
(define (foo x)
  (match x
    [(list x) x]))
------------------------------


test: "single-clause match still migratable when pattern bindings shadow surrounding context"
------------------------------
(define (foo x a)
  (match x
    [(list a b c) a]))
------------------------------
------------------------------
(define (foo x a)
  (match-define (list a b c) x)
  a)
------------------------------


test: "match patterns using ? with a lambda can be simplified with #:when clauses"
------------------------------
(define (foo x)
  (match x
    [(? (λ (y) (< y 10)) y*)
     (list y*)]
    [_ 'no-match]))
(foo 5)
(foo 100)
------------------------------
------------------------------
(define (foo x)
  (match x
    [y*
     #:when (< y* 10)
     (list y*)]
    [_ 'no-match]))
(foo 5)
(foo 100)
------------------------------


test: "nested match patterns using ? with a lambda can be simplified with #:when clauses"
------------------------------
(define (foo xs)
  (match xs
    [(list (? (λ (y) (< y 10)) y*))
     y*]
    [_ 'no-match]))
(foo (list 5 6 7))
(foo (list 100 200 300))
------------------------------
------------------------------
(define (foo xs)
  (match xs
    [(list y*)
     #:when (< y* 10)
     y*]
    [_ 'no-match]))
(foo (list 5 6 7))
(foo (list 100 200 300))
------------------------------


test: "match patterns using ? with a lambda cannot be simplified when under ellipses"
------------------------------
(define (foo xs)
  (match xs
    [(list (? (λ (y) (< y 10)) y*) ...)
     (list y*)]
    [_ 'no-match]))
------------------------------

test: "match patterns using ? with a commented lambda can be simplified with #:when clauses"
------------------------------
(define (foo x)
  (match x
    [(? (λ (y) (< y
                  10
                  ; comment
                  20)) y*)
     (list y*)]
    [_ 'no-match]))
(foo 5)
(foo 100)
------------------------------
------------------------------
(define (foo x)
  (match x
    [y*
     #:when (< y*
               10
               ; comment
               20)
     (list y*)]
    [_ 'no-match]))
(foo 5)
(foo 100)
------------------------------


test: "root-level and pattern can be removed when matching on a variable"
------------------------------
(define (f xs)
  (match xs
    [(and foo (list x y ...))
     (cons x (reverse foo))]
    [_ 'no-match]))
------------------------------
------------------------------
(define (f xs)
  (match xs
    [(list x y ...)
     (cons x (reverse xs))]
    [_ 'no-match]))
------------------------------


test: "and pattern removal works with multiple clauses"
------------------------------
(define (f xs)
  (match xs
    [(and items (list x y ...))
     (cons x (reverse items))]
    [(and value (list x))
     (list value x)]
    [_ 'no-match]))
------------------------------
------------------------------
(define (f xs)
  (match xs
    [(list x y ...)
     (cons x (reverse xs))]
    [(list x)
     (list xs x)]
    [_ 'no-match]))
------------------------------


test: "and pattern removal preserves other and patterns"
------------------------------
(define (f xs ys)
  (match xs
    [(and foo (list x y ...))
     (match ys
       [(and (list a b) (cons c d))
        (list foo a b c d)]
       [_ 'no-inner-match])]
    [_ 'no-match]))
------------------------------
------------------------------
(define (f xs ys)
  (match xs
    [(list x y ...)
     (match ys
       [(and (list a b) (cons c d))
        (list xs a b c d)]
       [_ 'no-inner-match])]
    [_ 'no-match]))
------------------------------


test: "and pattern with complex expression not refactorable"
------------------------------
(define (complex-expression) '(1 2 3))
(define (f)
  (match (complex-expression)
    [(and foo (list x y ...))
     (cons x (reverse foo))]
    [_ 'no-match]))
------------------------------


test: "nested and patterns not refactorable (yet)"
------------------------------
(define (f xs)
  (match xs
    [(list (and item x) y ...)
     (cons item (reverse xs))]
    [_ 'no-match]))
------------------------------


test: "if in match clause can be extracted as when guard"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (if (> x y)
         (list y x)
         pt)]
    [_ 'no-match]))
------------------------------
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     #:when (> x y)
     (list y x)]
    [(list x y) pt]
    [_ 'no-match]))
------------------------------


test: "cond in match clause can be extracted as when guard"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (cond
       [(> x y) (list y x)]
       [else pt])]
    [_ 'no-match]))
------------------------------
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     #:when (> x y)
     (list y x)]
    [(list x y) pt]
    [_ 'no-match]))
------------------------------


test: "if with additional body forms before and after"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (displayln "before")
     (if (> x y)
         (list y x)
         pt)
     (displayln "after")]
    [_ 'no-match]))
------------------------------
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     #:when (> x y)
     (displayln "before")
     (list y x)
     (displayln "after")]
    [(list x y)
     (displayln "before")
     pt
     (displayln "after")]
    [_ 'no-match]))
------------------------------


test: "single clause match with if not refactorable (would become match-define)"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (if (> x y)
         (list y x)
         pt)]))
------------------------------


test: "complex pattern with if not refactorable"
------------------------------
(define (f pt)
  (match pt
    [(list a b c d e f g)
     (if (> a b)
         (list b a c d e f g)
         pt)]
    [_ 'no-match]))
------------------------------

