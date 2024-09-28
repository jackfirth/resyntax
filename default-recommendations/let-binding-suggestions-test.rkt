#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations let-binding-suggestions


header:
- #lang racket/base


test: "empty let form"
------------------------------
(define (f)
  (let ()
    (displayln "foo")
    (displayln "foo")
    1))
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(define (f)
  (define x 1)
  x)
------------------------------


test: "single-clause let-values binding"
------------------------------
(define (f)
  (let-values ([(x y) (values 1 1)])
    (+ x y)))
------------------------------
------------------------------
(define (f)
  (define-values (x y) (values 1 1))
  (+ x y))
------------------------------


test: "single-clause single-identifier let-values binding"
------------------------------
(define (f)
  (let-values ([(x) 1])
    x))
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(define (f)
  (define-values (x y) (values 1 1))
  (+ x y))
------------------------------


test: "single-clause single-identifier let*-values binding"
------------------------------
(define (f)
  (let*-values ([(x) 1])
    x))
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(define (f)
  (define-values (x y) (values 1 1))
  (define-values (a b) (values 1 1))
  (+ x y a b))
------------------------------


test: "self-shadowing let binding isn't refactorable"
------------------------------
(define (f x)
  (let ([x (+ x 1)])
    1))
------------------------------


test: "self-shadowing let* binding isn't refactorable"
------------------------------
(define (f x)
  (let* ([x (+ x 1)])
    (* x 2)))
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


test: "let binding that only appears self shadowing before expansion (issue #230)"
------------------------------
(define (f b)
  (let ([x (let ([x 1]) (+ x 1))])
    (+ x 1)))
------------------------------
------------------------------
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
------------------------------
------------------------------
(define (f a)
  (define b (+ a 1))
  (define c (+ b 1))
  (define d (+ c 1))
  d)
------------------------------


test: "let* with later bindings shadowing earlier right-hand-sides not refactorable"
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
------------------------------
------------------------------
(define (f)
  (let* ([x (println "foo")])
    42))
------------------------------
------------------------------
(define (f)
  (let-values ([(x) (println "foo")])
    42))
------------------------------
------------------------------
(define (f)
  (let*-values ([(x) (println "foo")])
    42))
------------------------------
------------------------------
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
------------------------------
------------------------------
(define (f)
  (println "foo")
  (define x 42)
  x)
------------------------------


test: "let forms inside lambdas"
------------------------------
(λ ()
  (let ([x 1])
    x))
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(when #true
  (define x 1)
  x)
------------------------------


test: "let forms inside unless forms"
------------------------------
(unless #false
  (let ([x 1])
    x))
------------------------------
------------------------------
(unless #false
  (define x 1)
  x)
------------------------------


test: "let forms inside with-handlers forms"
------------------------------
(with-handlers ([exn:fail? void])
  (let ([x 1])
    x))
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(for ([i (in-range 0 10)])
  (define x 1)
  (displayln x))
------------------------------


test: "let forms at module level not refactorable to define"
------------------------------
(let ([x 1])
  (* x 2))
------------------------------


test: "let forms at submodule level not refactorable to define"
------------------------------
(module+ test
  (let ([x 1])
    (* x 2)))
------------------------------


test: "named lets which don't refer to the name are refactorable to unnamed lets"
------------------------------
(let loop ([x 1]) x)
------------------------------
------------------------------
(let ([x 1]) x)
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


test: "let binding with conflicting define inside"
------------------------------
(define (g)
  (let* ([x 1]
         [y x])
    (define x 2)
    (+ x y)))
------------------------------


test: "let binding with obfuscated conflicting define inside"
------------------------------
(define (g)
  (let* ([x 'outer]
         [y x])
    (displayln y)
    (define-syntax-rule (m a)
      (begin
        (define a 'inner)
        x))
    (m x)))
------------------------------


test: "variable definition with nested let binding refactorable to two variable definitions"
------------------------------
(define (f)
  (displayln "foo")
  (define y (let ([x 1]) (* x 2)))
  (* y 3))
------------------------------
------------------------------
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
------------------------------
------------------------------
(define (f c)
  (cond
    [c
     (displayln "foo")
     (define x 1)
     (define y (* x 2))
     (* y 3)]
    [else (displayln "else")]))
------------------------------


test: "variable definition with nested let binding of same name not refactorable"
------------------------------
(define (f)
  (define y (let ([y 1]) (* y 2)))
  (* y 3))
------------------------------


test: "variable definition with nested let binding of name bound later not refactorable"
------------------------------
(define (f)
  (define y (let ([x 1]) (* x 2)))
  (define x 5)
  (* y 3))
------------------------------


test: "variable definition with nested let binding of name bound earlier not refactorable"
------------------------------
(define (f)
  (define x 5)
  (define y (let ([x 1]) (* x 2)))
  (* y 3))
------------------------------


test: "variable definition with nested let binding shadowing name used later not refactorable"
------------------------------
(define x 5)
(define (f)
  (define y (let ([x 1]) (* x 2)))
  (* x y))
------------------------------


test: "let binding nested in begin0 extractable to definition"
------------------------------
(define (f)
  (begin0 (let ([x 1]) x)
    (displayln "foo")))
------------------------------
------------------------------
(define (f)
  (define x 1)
  (begin0 x
    (displayln "foo")))
------------------------------


test: "redundant let bindings can be removed"
------------------------------
(define x 1)
(let ([x x])
  (* x 2))
------------------------------
------------------------------
(define x 1)
(* x 2)
------------------------------
