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
