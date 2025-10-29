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
==============================
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
==============================
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
==============================
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
"single-clause match not migratable when pattern bindings conflict with surrounding context"
------------------------------
(define (foo x)
  (define a 42)
  (match x
    [(list a b c) a]))
------------------------------


no-change-test: "single-clause match not migratable when pattern would bind subject expression"
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
==============================
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
==============================
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
==============================
(define (foo xs)
  (match xs
    [(list y*)
     #:when (< y* 10)
     y*]
    [_ 'no-match]))
(foo (list 5 6 7))
(foo (list 100 200 300))
------------------------------


no-change-test: "match patterns using ? with a lambda cannot be simplified when under ellipses"
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
==============================
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
    [(and foo (list x y ...)) (cons x (reverse foo))]
    [(list) (list)]))
==============================
(define (f xs)
  (match xs
    [(list x y ...) (cons x (reverse xs))]
    [(list) (list)]))
------------------------------


test: "root-level and pattern can be removed when it binds unused variable"
------------------------------
(define (f xs)
  (match xs
    [(and foo (list x y ...)) (cons x y)]
    [(list) (list)]))
==============================
(define (f xs)
  (match xs
    [(list x y ...) (cons x y)]
    [(list) (list)]))
------------------------------


no-change-test: "root-level and pattern not removed when not matching on a simple variable"
------------------------------
(define (f lst)
  (match (car lst)
    [(and foo (list x y ...)) (cons x (reverse foo))]
    [(list) (list)]))
------------------------------


test: "match patterns with long if conditionals can be simplified using #:when clauses"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (if (> x y)
         (list y x 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo)
         pt)]
    [(list) '()]))
==============================
(define (f pt)
  (match pt
    [(list x y)
     #:when (> x y)
     (list y x 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo)]
    [(list x y) pt]
    [(list) '()]))
------------------------------


no-change-test: "match patterns with short if conditionals won't be simplified using #:when clauses"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (if (> x y)
         (list y x)
         pt)]
    [(list) '()]))
------------------------------


test: "match patterns with long cond conditionals can be simplified using #:when clauses"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (cond
       [(> x y) (list y x 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo)]
       [else pt])]
    [(list) '()]))
==============================
(define (f pt)
  (match pt
    [(list x y)
     #:when (> x y)
     (list y x 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo 'foo)]
    [(list x y) pt]
    [(list) '()]))
------------------------------


no-change-test: "match patterns with short cond conditionals won't be simplified using #:when clauses"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (cond
       [(> x y) (list y x)]
       [else pt])]
    [(list) '()]))
------------------------------

test: "match patterns with multi-body cond conditionals can be simplified using #:when clauses"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (cond
       [(> x y)
        (displayln "true case")
        (list y x)]
       [else
        (displayln "false case")
        pt])]
    [(list) '()]))
==============================
(define (f pt)
  (match pt
    [(list x y)
     #:when (> x y)
     (displayln "true case")
     (list y x)]
    [(list x y)
     (displayln "false case")
     pt]
    [(list) '()]))
------------------------------


no-change-test: "match patterns with cond conditionals that use => can't be simplified using #:when"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (cond
       [(> x y) => (λ (bool) (list y x))]
       [else pt])]
    [(list) '()]))
------------------------------


test: "single-clause match with if conditional should be refactored to match-define instead of #:when"
------------------------------
(define (f pt)
  (match pt
    [(list x y)
     (if (> x y)
         (list y x)
         pt)]))
==============================
(define (f pt)
  (match-define (list x y) pt)
  (if (> x y)
      (list y x)
      pt))
------------------------------


no-change-test: "match with if conditional and long pattern should not be refactored to use #:when"
------------------------------
(define (f data)
  (match data
    [(list x y _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _ _)
     (if (> x y)
         (list y x)
         data)]
    [_ 'no-match]))
------------------------------


test: "list element variable definitions refactorable to match-define"
------------------------------
(require racket/list)
(define (f pt)
  (define x (list-ref pt 0))
  (define y (list-ref pt 1))
  (define z (list-ref pt 2))
  (+ x y z))
==============================
(require racket/list)
(define (f pt)
  (define x (first pt))
  (define y (second pt))
  (define z (third pt))
  (+ x y z))
==============================
(require racket/list)
(define (f pt)
  (match-define (list x y z) pt)
  (+ x y z))
------------------------------


no-change-test: "list element variable definitions not refactorable when list is used before"
------------------------------
(require racket/list)
(define (f pt)
  (displayln (drop pt 3))
  (define x (list-ref pt 0))
  (define y (list-ref pt 1))
  (define z (list-ref pt 2))
  (+ x y z))
------------------------------


no-change-test: "list element variable definitions not refactorable when list is used after"
------------------------------
(require racket/list)
(define (f pt)
  (define x (list-ref pt 0))
  (define y (list-ref pt 1))
  (define z (list-ref pt 2))
  (displayln (drop pt 3))
  (+ x y z))
------------------------------


no-change-test: "list element variable definitions not refactorable when out of order"
------------------------------
(define (f pt)
  (define z (list-ref pt 2))
  (define y (list-ref pt 1))
  (define x (list-ref pt 0))
  (+ x y z))
------------------------------


no-change-test: "list element variable definitions not refactorable unless starting at zero"
------------------------------
(define (f pt)
  (define x (list-ref pt 1))
  (define y (list-ref pt 2))
  (define z (list-ref pt 3))
  (+ x y z))
------------------------------


no-change-test: "list element variable definitions not refactorable when referencing different lists"
------------------------------
(define (f pt1 pt2)
  (define x (list-ref pt1 0))
  (define y (list-ref pt2 1))
  (define z (list-ref pt1 2))
  (+ x y z))
------------------------------


no-change-test: "list element variable definitions not refactorable when referencing list expressions"
------------------------------
(define (f pt-list)
  (define x (list-ref (first pt-list) 0))
  (define y (list-ref (first pt-list) 1))
  (define z (list-ref (first pt-list) 2))
  (+ x y z))
------------------------------


test: "refactoring list element variable definitions to match-define doesn't reformat surroundings"
------------------------------
(require racket/list)
(define (f pt)

  ( void )
  
  (define x (list-ref pt 0))
  (define y (list-ref pt 1))
  (define z (list-ref pt 2))

  ; comment
  ( + x y z ))
==============================
(require racket/list)
(define (f pt)

  ( void )
  
  (match-define (list x y z) pt)

  ; comment
  ( + x y z ))
------------------------------


test: "and with match on same identifier can be simplified"
------------------------------
(define (f x)
  (and x (match x [1 2])))
==============================
(define (f x)
  (match x
    [#f #f]
    [1 2]))
------------------------------


test: "and with match on same identifier preserves formatting"
------------------------------
(define (foo some-var)
  (and some-var
       (match some-var
         ['first-case 'first-result]
         ['second-case 'second-result])))
==============================
(define (foo some-var)
  (match some-var
    [#f #f]
    ['first-case 'first-result]
    ['second-case 'second-result]))
------------------------------


no-change-test: "and with match on different identifiers not refactorable"
- (define (foo x y) (and x (match y ['a 'b] ['c 'd])))

