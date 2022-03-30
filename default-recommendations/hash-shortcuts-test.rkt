#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations hash-shortcuts


header:
- #lang racket/base


test: "hash-ref with constant lambda can be simplified to hash-ref without lambda"
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref h k (λ () 42))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref h k 42)
------------------------------


test: "hash-ref with non-constant lambda cannot be simplified to hash-ref without lambda"
------------------------------
(define h (make-hash))
(define k 'a)
(define (f x) x)
(hash-ref h k (λ () (f 42)))
------------------------------


test: "hash-ref! with constant lambda can be simplified to hash-ref! without lambda"
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref! h k (λ () 42))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref! h k 42)
------------------------------


test: "hash-ref! with non-constant lambda cannot be simplified to hash-ref! without lambda"
------------------------------
(define h (make-hash))
(define k 'a)
(define (f x) x)
(hash-ref! h k (λ () (f 42)))
------------------------------


test: "hash-ref with hash-set! lambda can be simplified to hash-ref!"
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref h k (λ ()
                (define v (+ 1 2 3))
                (hash-set! h k v)
                v))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref! h k (λ () (+ 1 2 3)))
------------------------------


test: "hash-ref with hash-set! lambda with constant can be simplified to hash-ref!"
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref h k (λ ()
                (hash-set! h k 0)
                0))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref h k (λ ()
                (define v 0)
                (hash-set! h k v)
                v))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref! h k 0)
------------------------------


test: "hash-ref with hash-set! lambda with thunk can be simplified to hash-ref!"
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref h k (λ ()
                (define v (make-hash))
                (hash-set! h k v)
                v))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref! h k make-hash)
------------------------------
