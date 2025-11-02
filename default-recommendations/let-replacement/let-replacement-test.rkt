#lang resyntax/test


require: resyntax/default-recommendations let-replacement


header:
- #lang racket/base


test: "empty let form"
------------------------------
(define (f)
  (let ()
    (displayln "foo")
    (displayln "foo")
    1))
==============================
(define (f)
  (displayln "foo")
  (displayln "foo")
  1)
------------------------------


test: "single let binding"
------------------------------
(define (f)
  (let ([x 1])
    x))
==============================
(define (f)
  (define x 1)
  x)
------------------------------


test: "single let binding inside cond"
------------------------------
(define (f c)
  (cond
    [c
     (let ([x 1])
       x)]
    [else (displayln "else")]))
==============================
(define (f c)
  (cond
    [c
     (define x 1)
     x]
    [else (displayln "else")]))
------------------------------


test: "single let* binding"
------------------------------
(define (f)
  (let* ([x 1])
    x))
==============================
(define (f)
  (define x 1)
  x)
------------------------------


test: "single-clause let-values binding"
------------------------------
(define (f)
  (let-values ([(x y) (values 1 1)])
    (+ x y)))
==============================
(define (f)
  (define-values (x y) (values 1 1))
  (+ x y))
------------------------------


test: "single-clause single-identifier let-values binding"
------------------------------
(define (f)
  (let-values ([(x) 1])
    x))
==============================
(define (f)
  (define x 1)
  x)
------------------------------


test: "single-clause single-identifier multiline let-values binding"
------------------------------
(define (f)
  (let-values ([(x)
                (list 1000000000000000000000000000000000
                      2000000000000000000000000000000000
                      3000000000000000000000000000000000)])
    x))
==============================
(define (f)
  (define x
    (list 1000000000000000000000000000000000
          2000000000000000000000000000000000
          3000000000000000000000000000000000))
  x)
------------------------------


test: "single-clause let*-values binding"
------------------------------
(define (f)
  (let*-values ([(x y) (values 1 1)])
    (+ x y)))
==============================
(define (f)
  (define-values (x y) (values 1 1))
  (+ x y))
------------------------------


test: "single-clause single-identifier let*-values binding"
------------------------------
(define (f)
  (let*-values ([(x) 1])
    x))
==============================
(define (f)
  (define x 1)
  x)
------------------------------


test: "single-clause single-identifier multiline let*-values binding"
------------------------------
(define (f)
  (let*-values ([(x)
                 (list 1000000000000000000000000000000000
                       2000000000000000000000000000000000
                       3000000000000000000000000000000000)])
    x))
==============================
(define (f)
  (define x
    (list 1000000000000000000000000000000000
          2000000000000000000000000000000000
          3000000000000000000000000000000000))
  x)
------------------------------


test: "multiple let bindings"
------------------------------
(define (f)
  (let ([x 1]
        [y 1])
    (+ x y)))
==============================
(define (f)
  (define x 1)
  (define y 1)
  (+ x y))
------------------------------


test: "multiple let* bindings"
------------------------------
(define (f)
  (let* ([x 1]
         [y 1])
    (+ x y)))
==============================
(define (f)
  (define x 1)
  (define y 1)
  (+ x y))
------------------------------


test: "multiple let-values bindings"
------------------------------
(define (f)
  (let-values ([(x y) (values 1 1)]
               [(a b) (values 1 1)])
    (+ x y a b)))
==============================
(define (f)
  (define-values (x y) (values 1 1))
  (define-values (a b) (values 1 1))
  (+ x y a b))
------------------------------


test: "multiple let*-values bindings"
------------------------------
(define (f)
  (let*-values ([(x y) (values 1 1)]
                [(a b) (values 1 1)])
    (+ x y a b)))
==============================
(define (f)
  (define-values (x y) (values 1 1))
  (define-values (a b) (values 1 1))
  (+ x y a b))
------------------------------


no-change-test: "self-shadowing let binding isn't refactorable"
------------------------------
(define (f x)
  (let ([x (+ x 1)])
    1))
------------------------------


no-change-test: "self-shadowing let* binding isn't refactorable"
------------------------------
(define (f x)
  (let* ([x (+ x 1)])
    (* x 2)))
------------------------------


no-change-test: "self-shadowing let-values binding clause isn't refactorable"
------------------------------
(define (f x)
  (let-values ([(x y) (values x 1)])
    1))
------------------------------


no-change-test: "self-shadowing let*-values binding clause isn't refactorable"
------------------------------
(define (f x)
  (let*-values ([(x y) (values x 1)])
    1))
------------------------------


test: "let binding that only appears self shadowing before expansion (issue #230)"
------------------------------
(define (f b)
  (let ([x (let ([x 1]) (+ x 1))])
    (+ x 1)))
==============================
(define (f b)
  (define x (let ([x 1]) (+ x 1)))
  (+ x 1))
------------------------------


test: "let* with later right-hand-sides referring to earlier bindings is refactorable"
------------------------------
(define (f a)
  (let* ([b (+ a 1)]
         [c (+ b 1)]
         [d (+ c 1)])
    d))
==============================
(define (f a)
  (define b (+ a 1))
  (define c (+ b 1))
  (define d (+ c 1))
  d)
------------------------------


no-change-test: "let* with later bindings shadowing earlier right-hand-sides not refactorable"
------------------------------
(define y 1)
(define (f)
  (let* ([x (+ y 1)]
         [y (+ x 1)])
    (* x y)))
------------------------------


test: "unused let binding is refactorable to side-effectful expression"
------------------------------
(define (f)
  (let ([x (println "foo")])
    42))
==============================
(define (f)
  (let* ([x (println "foo")])
    42))
==============================
(define (f)
  (let-values ([(x) (println "foo")])
    42))
==============================
(define (f)
  (let*-values ([(x) (println "foo")])
    42))
==============================
(define (f)
  (println "foo")
  42)
------------------------------


test: "unused let* binding shadowed by later bindings is refactorable to side-effectful expression"
------------------------------
(define (f)
  (let* ([x (println "foo")]
         [x 42])
    x))
==============================
(define (f)
  (println "foo")
  (define x 42)
  x)
------------------------------


test: "partially used let-values binding refactorable to define-values"
------------------------------
(define (f)
  (let-values ([(x y) (values 1 2)])
    x))
==============================
(define (f)
  (define-values (x y) (values 1 2))
  x)
------------------------------


test: "partially used let-values binding at phase 1 refactorable to define-values"
------------------------------
(require (for-syntax racket/base))
(define-syntaxes (a b c)
  (let ()
    (define (f)
      (let-values ([(x y) (values 1 2)])
        x))
    (values 1 2 3)))
==============================
(require (for-syntax racket/base))
(define-syntaxes (a b c)
  (let ()
    (define (f)
      (define-values (x y) (values 1 2))
      x)
    (values 1 2 3)))
------------------------------


no-change-test: "let forms with conflicting outer definitions not refactorable"
------------------------------
(define (f)
  (define x 1)
  (let ([x 2])
    x))
------------------------------


no-change-test: "let forms with conflicting outer definitions at phase 1 not refactorable"
------------------------------
(require (for-syntax racket/base))
(begin-for-syntax
  (define (f)
    (define x 1)
    (let ([x 2])
      x)))
------------------------------


test: "let forms inside lambdas"
------------------------------
(λ ()
  (let ([x 1])
    x))
==============================
(λ ()
  (define x 1)
  x)
------------------------------


test: "let forms inside unrefactorable let forms"
------------------------------
(define a 1)
(let ([a (+ a 1)])
  (let ([x 1])
    x))
==============================
(define a 1)
(let ([a (+ a 1)])
  (define x 1)
  x)
------------------------------


test: "let forms inside let loops"
------------------------------
(let loop ()
  (let ([x 1])
    x))
==============================
(let loop ()
  (define x 1)
  x)
------------------------------


test: "let forms inside unrefactorable let* forms"
------------------------------
(define a 1)
(let* ([a a]
       [a a])
  (let ([x 1])
    x))
==============================
(define a 1)
(let* ([a a]
       [a a])
  (define x 1)
  x)
------------------------------


test: "let forms inside unrefactorable let-values forms"
------------------------------
(define a 1)
(let-values ([(a b) (values a 1)])
  (let ([x 1])
    x))
==============================
(define a 1)
(let-values ([(a b) (values a 1)])
  (define x 1)
  x)
------------------------------


test: "let forms inside unrefactorable let*-values forms"
------------------------------
(define a 1)
(let*-values ([(a b) (values a 1)])
  (let ([x 1])
    x))
==============================
(define a 1)
(let*-values ([(a b) (values a 1)])
  (define x 1)
  x)
------------------------------


test: "let forms inside when forms"
------------------------------
(when #true
  (let ([x 1])
    x))
==============================
(when #true
  (define x 1)
  x)
------------------------------


test: "let forms inside unless forms"
------------------------------
(unless #false
  (let ([x 1])
    x))
==============================
(unless #false
  (define x 1)
  x)
------------------------------


test: "let forms inside with-handlers forms"
------------------------------
(with-handlers ([exn:fail? void])
  (let ([x 1])
    x))
==============================
(with-handlers ([exn:fail? void])
  (define x 1)
  x)
------------------------------


test: "let forms inside parameterize forms"
------------------------------
(define p (make-parameter #false))
(parameterize ([p #true])
  (let ([x 1])
    x))
==============================
(define p (make-parameter #false))
(parameterize ([p #true])
  (define x 1)
  x)
------------------------------


test: "let forms inside for loop bodies"
------------------------------
(for ([i (in-range 0 10)])
  (let ([x 1])
    (displayln x)))
==============================
(for ([i (in-range 0 10)])
  (define x 1)
  (displayln x))
------------------------------


no-change-test: "let forms at module level not refactorable to define"
------------------------------
(let ([x 1])
  (* x 2))
------------------------------


no-change-test: "let forms at submodule level not refactorable to define"
------------------------------
(module+ test
  (let ([x 1])
    (* x 2)))
------------------------------


test: "variable definition with nested let binding refactorable to two variable definitions"
------------------------------
(define (f)
  (displayln "foo")
  (define y (let ([x 1]) (* x 2)))
  (* y 3))
==============================
(define (f)
  (displayln "foo")
  (define x 1)
  (define y (* x 2))
  (* y 3))
------------------------------


test:
"variable definition with nested let binding inside cond refactorable to two variable definitions"
------------------------------
(define (f c)
  (cond
    [c
     (displayln "foo")
     (define y (let ([x 1]) (* x 2)))
     (* y 3)]
    [else (displayln "else")]))
==============================
(define (f c)
  (cond
    [c
     (displayln "foo")
     (define x 1)
     (define y (* x 2))
     (* y 3)]
    [else (displayln "else")]))
------------------------------


no-change-test: "variable definition with nested let binding of same name not refactorable"
------------------------------
(define (f)
  (define y (let ([y 1]) (* y 2)))
  (* y 3))
------------------------------


no-change-test: "variable definition with nested let binding of name bound later not refactorable"
------------------------------
(define (f)
  (define y (let ([x 1]) (* x 2)))
  (define x 5)
  (* y 3))
------------------------------


no-change-test: "variable definition with nested let binding of name bound earlier not refactorable"
------------------------------
(define (f)
  (define x 5)
  (define y (let ([x 1]) (* x 2)))
  (* y 3))
------------------------------


no-change-test:
"variable definition with nested let binding shadowing name used later not refactorable"
------------------------------
(define x 5)
(define (f)
  (define y (let ([x 1]) (* x 2)))
  (* x y))
------------------------------


test: "variable definition with nested let binding with multiple bindings refactorable"
------------------------------
(define (f)
  (displayln "foo")
  (define a (let ([b 1] [c 2]) (+ b c 10)))
  (* a 3))
==============================
(define (f)
  (displayln "foo")
  (define b 1)
  (define c 2)
  (define a (+ b c 10))
  (* a 3))
------------------------------


test: "variable definition with nested let binding with three bindings refactorable"
------------------------------
(define (f)
  (define a (let ([b 1] [c 2] [d 3]) (+ b c d)))
  (* a 10))
==============================
(define (f)
  (define b 1)
  (define c 2)
  (define d 3)
  (define a (+ b c d))
  (* a 10))
------------------------------


no-change-test:
"variable definition with nested let binding with multiple bindings where one conflicts not refactorable"
------------------------------
(define (f)
  (define x 5)
  (define a (let ([b 1] [x 2]) (+ b x 10)))
  (* a 3))
------------------------------


no-change-test:
"variable definition with nested let binding with multiple bindings where one shadows outer binding not refactorable"
------------------------------
(define x 5)
(define (f)
  (define a (let ([x 1] [c 2]) (+ x c 10)))
  (* x a))
------------------------------


test: "let binding nested in begin0 extractable to definition"
------------------------------
(define (f)
  (begin0 (let ([x 1]) x)
    (displayln "foo")))
==============================
(define (f)
  (define x 1)
  (begin0 x
    (displayln "foo")))
------------------------------


test: "let binding with body nested in begin0 extractable to definition and body"
------------------------------
(define (f)
  (begin0 (let ([x 1])
            (displayln "foo")
            x)
    (displayln "bar")))
==============================
(define (f)
  (define x 1)
  (displayln "foo")
  (begin0 x
    (displayln "bar")))
------------------------------


test: "let-to-define doesn't reformat the entire definition context"
----------------------------------------
(define (f)
  ( displayln "foo" )
  (let ([x 1])
    (* x 2)))
========================================
(define (f)
  ( displayln "foo" )
  (define x 1)
  (* x 2))
----------------------------------------


test: "define-let-to-multi-define doesn't reformat the entire definition context"
----------------------------------------
(define (f)
  ( displayln "foo" )
  (define y (let ([x 1]) (* x 2)))
  ( displayln "bar" ))
========================================
(define (f)
  ( displayln "foo" )
  (define x 1)
  (define y (* x 2))
  ( displayln "bar" ))
----------------------------------------

