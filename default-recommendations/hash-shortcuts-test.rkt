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
(define (f x)
  x)
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
(define (f x)
  x)
(hash-ref! h k (λ () (f 42)))
------------------------------


test: "hash-ref with hash-set! can be simplified to hash-ref!"
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
(or (hash-ref h k #false)
    (let ([v (+ 1 2 3)])
      (hash-set! h k v)
      v))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-ref! h k (λ () (+ 1 2 3)))
------------------------------


test: "hash-ref with hash-set! and literal keys can be simplified to hash-ref!"
------------------------------
(define h (make-hash))
(hash-ref h 'a (λ ()
                 (define v (+ 1 2 3))
                 (hash-set! h 'a v)
                 v))
------------------------------
------------------------------
(define h (make-hash))
(or (hash-ref h 'a #false)
    (let ([v (+ 1 2 3)])
      (hash-set! h 'a v)
      v))
------------------------------
------------------------------
(define h (make-hash))
(hash-ref! h 'a (λ () (+ 1 2 3)))
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


test: "hash-set! with hash-ref can be simplified to hash-update!"
------------------------------
(define h (make-hash))
(define k 'a)
(hash-set! h k (+ 5 (hash-ref h k 0)))
------------------------------
------------------------------
(define h (make-hash))
(define k 'a)
(hash-update! h k (λ (v) (+ 5 v)) 0)
------------------------------


test: "hash-set! with hash-ref and literal keys can be simplified to hash-update!"
------------------------------
(define h (make-hash))
(hash-set! h 'a (+ 5 (hash-ref h 'a 0)))
------------------------------
------------------------------
(define h (make-hash))
(hash-update! h 'a (λ (v) (+ 5 v)) 0)
------------------------------


test: "hash-set! with hash-ref cannot be simplified when v would shadow"
------------------------------
(define h (make-hash))
(define k 'a)
(define v 5)
(hash-set! h k (+ v (hash-ref h k 0)))
------------------------------


test: "hash-map with key-returning lamda can be refactored to hash-keys"
------------------------------
(define h (make-hash))
(hash-map h (λ (k v) k))
------------------------------
------------------------------
(define h (make-hash))
(hash-keys h)
------------------------------


test: "hash-map with value-returning lamda can be refactored to hash-values"
------------------------------
(define h (make-hash))
(hash-map h (λ (k v) v))
------------------------------
------------------------------
(define h (make-hash))
(hash-values h)
------------------------------
