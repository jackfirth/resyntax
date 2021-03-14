#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations hash-shortcuts


test: "hash-ref with hash-set! lambda can be simplified to hash-ref!"
------------------------------
#lang racket/base
(define h (make-hash))
(define k 'a)
(hash-ref h k (λ ()
                (define v 5)
                (hash-set! h k v)
                v))
------------------------------
------------------------------
#lang racket/base
(define h (make-hash))
(define k 'a)
(hash-ref! h k (λ () 5))
------------------------------
