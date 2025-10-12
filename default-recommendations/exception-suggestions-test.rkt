#lang resyntax/test


require: resyntax/default-recommendations exception-suggestions


header:
--------------------
#lang racket/base
(define (do-thing) (void))
(define (handle-error e) (void))
--------------------


test: "with-handlers with constant handler refactorable to lambda"
--------------------
(define (try-to-do-thing)
  (with-handlers ([exn:fail? #f])
    (do-thing)))
====================
(define (try-to-do-thing)
  (with-handlers ([exn:fail? (lambda (_) #f)])
    (do-thing)))
--------------------


test: "with-handlers with number handler refactorable to lambda"
--------------------
(with-handlers ([exn:fail? 42])
  (do-thing))
====================
(with-handlers ([exn:fail? (lambda (_) 42)])
  (do-thing))
--------------------


test: "with-handlers with string handler refactorable to lambda"
--------------------
(with-handlers ([exn:fail? "error"])
  (do-thing))
====================
(with-handlers ([exn:fail? (lambda (_) "error")])
  (do-thing))
--------------------


test: "with-handlers with quoted literal handler refactorable to lambda"
--------------------
(with-handlers ([exn:fail? 'symbol])
  (do-thing))
====================
(with-handlers ([exn:fail? (lambda (_) 'symbol)])
  (do-thing))
--------------------


test: "with-handlers with multiple clauses, first has literal"
--------------------
(with-handlers ([exn:fail? #f]
                [exn:break? (lambda (e) e)])
  (do-thing))
====================
(with-handlers ([exn:fail? (lambda (_) #f)]
                [exn:break? (lambda (e) e)])
  (do-thing))
--------------------


test: "with-handlers with multiple clauses, second has literal"
--------------------
(with-handlers ([exn:fail? (lambda (e) e)]
                [exn:break? #f])
  (do-thing))
====================
(with-handlers ([exn:fail? (lambda (e) e)]
                [exn:break? (lambda (_) #f)])
  (do-thing))
--------------------


no-change-test: "with-handlers with procedure handler not refactorable"
--------------------
(with-handlers ([exn:fail? (lambda (e) #f)])
  (do-thing))
--------------------


no-change-test: "with-handlers with identifier handler not refactorable"
--------------------
(with-handlers ([exn:fail? handle-error])
  (do-thing))
--------------------
