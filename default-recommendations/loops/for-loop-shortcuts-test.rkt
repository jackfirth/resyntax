#lang resyntax/test


require: resyntax/default-recommendations for-loop-shortcuts


header:
- #lang racket/base


no-change-test: "hash-for-each with short single-body form not refactorable"
------------------------------
(define some-hash (hash 'a 1 'b 2))
(hash-for-each some-hash (λ (k v) (displayln v)))
------------------------------


test: "hash-for-each with long single-body form refactorable to for"
------------------------------
(define some-hash (hash 'a 1 'b 2))
(hash-for-each
 some-hash
 (λ (k a-very-very-very-long-variable-name-thats-so-very-long)
   (displayln a-very-very-very-long-variable-name-thats-so-very-long)))
==============================
(define some-hash (hash 'a 1 'b 2))
(for ([(k a-very-very-very-long-variable-name-thats-so-very-long) (in-hash some-hash)])
  (displayln a-very-very-very-long-variable-name-thats-so-very-long))
------------------------------


test: "hash-for-each with multiple body forms refactorable to for"
------------------------------
(define some-hash (hash 'a 1 'b 2))
(hash-for-each
 some-hash
 (λ (k v)
   (displayln k)
   (displayln v)))
==============================
(define some-hash (hash 'a 1 'b 2))
(for ([(k v) (in-hash some-hash)])
  (displayln k)
  (displayln v))
------------------------------


test: "hash-for-each with let expression refactorable to for with definitions"
------------------------------
(define some-hash (hash 1 10 2 20))
(hash-for-each
 some-hash
 (λ (k v)
   (let ([x (+ k v)])
     (displayln x))))
==============================
(define some-hash (hash 1 10 2 20))
(for ([(k v) (in-hash some-hash)])
  (define x (+ k v))
  (displayln x))
------------------------------


test: "for/and with or guarding complex expression to filter clause"
------------------------------
(define some-list (list 3 "foo" 5 14 "bar" 10 6 "baz" 5 2))
(for/and ([x (in-list some-list)])
  (or (number? x)
      (let ([l (string-length x)])
        (and (odd? l) (< l 10)))))
==============================
(define some-list (list 3 "foo" 5 14 "bar" 10 6 "baz" 5 2))
(for/and ([x (in-list some-list)]
          #:unless (number? x))
  (define l (string-length x))
  (and (odd? l) (< l 10)))
------------------------------


no-change-test: "for/and with or guarding simple expression not refactorable"
------------------------------
(define some-list (list 3 "foo" 5 14 "bar" 10 6 "baz" 5 2))
(for/and ([x (in-list some-list)])
  (or (number? x)
      (string? x)))
------------------------------


test: "for with set! refactorable to for/fold"
------------------------------
(define (f)
  (define xs '())
  (for ([i (in-range 0 10)])
    (define j (* i 2))
    (set! xs (cons j xs)))
  (reverse xs))
==============================
(define (f)
  (define xs
    (for/fold ([xs '()]) ([i (in-range 0 10)])
      (define j (* i 2))
      (cons j xs)))
  (reverse xs))
------------------------------


test: "for/fold building hash to for/hash"
------------------------------
(for/fold ([h (hash)])
          ([x (in-range 0 10)])
  (hash-set h x 'foo))
==============================
(for/hash ([x (in-range 0 10)])
  (values x 'foo))
------------------------------


test: "for*/fold building hash to for*/hash"
------------------------------
(for*/fold ([h (hash)])
           ([x (in-range 0 10)])
  (hash-set h x 'foo))
==============================
(for*/hash ([x (in-range 0 10)])
  (values x 'foo))
------------------------------


no-change-test: "for/fold building hash can't be refactored when referring to hash"
------------------------------
(for/fold ([h (hash)])
          ([x (in-range 0 10)])
  (displayln (hash-has-key? h x))
  (hash-set h x 'foo))
------------------------------


no-change-test: "for*/fold building hash can't be refactored when referring to hash"
------------------------------
(for*/fold ([h (hash)])
           ([x (in-range 0 10)])
  (displayln (hash-has-key? h x))
  (hash-set h x 'foo))
------------------------------


test: "multi-accumulator for/fold with one used result refactorable to for/fold using #:result"
------------------------------
(define (foo)
  (define-values (x y z)
    (for/fold ([accum1 0]
               [accum2 0]
               [accum3 0])
              ([n (in-naturals)])
      (values 0 0 0)))
  (* x 2))
==============================
(define (foo)
  (define x
    (for/fold ([accum1 0]
               [accum2 0]
               [accum3 0]
               #:result accum1)
              ([n (in-naturals)])
      (values 0 0 0)))
  (* x 2))
------------------------------


test: "multi-accumulator for/fold with one used result refactorable without formatting surroundings"
------------------------------
(define (foo)
  (  displayln   "foo"   )

  (define-values (x y z)
    (for/fold ([accum1 0]
               [accum2 0]
               [accum3 0])
              ([n (in-naturals)])
      (values 0 0 0)))

  (*   x   2))
==============================
(define (foo)
  (  displayln   "foo"   )

  (define x
    (for/fold ([accum1 0]
               [accum2 0]
               [accum3 0]
               #:result accum1)
              ([n (in-naturals)])
      (values 0 0 0)))

  (*   x   2))
------------------------------


test: "for/fold with conditional body refactorable to for/fold with #:when"
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (if (even? n)
        (+ accum n)
        accum)))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (if (not (even? n))
        accum
        (+ accum n))))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) (+ accum n)]
      [else accum])))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(not (even? n)) accum]
      [else (+ accum n)])))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) (+ accum n)]
      [accum])))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)]
             #:when (even? n))
    (+ accum n)))
------------------------------


test: "for/fold with negated conditional body refactorable to for/fold with #:unless"
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (if (even? n)
        accum
        (+ accum n))))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (if (not (even? n))
        (+ accum n)
        accum)))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) accum]
      [else (+ accum n)])))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(not (even? n)) (+ accum n)]
      [else accum])))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) accum]
      [(+ accum n)])))
==============================
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)]
             #:unless (even? n))
    (+ accum n)))
------------------------------


test: "nested for forms can be flattened to a for* form"
------------------------------
(for ([x (in-range 0 5)])
  (for ([y (in-range 0 5)])
    (for ([z (in-range 0 5)])
      (displayln x)
      (displayln y)
      (displayln z))))
==============================
(for* ([x (in-range 0 5)]
       [y (in-range 0 5)]
       [z (in-range 0 5)])
  (displayln x)
  (displayln y)
  (displayln z))
------------------------------


no-change-test: "non-nested for form isn't replaced by a for* form"
------------------------------
(for ([x (in-range 0 5)])
  (displayln x)
  (displayln x)
  (displayln x))
------------------------------


test: "nested for/or forms can be flattened to a for*/or form"
------------------------------
(for/or ([x (in-range 0 5)])
  (for/or ([y (in-range 0 5)])
    (for/or ([z (in-range 0 5)])
      (>= (+ x y z) 5))))
==============================
(for*/or ([x (in-range 0 5)]
          [y (in-range 0 5)]
          [z (in-range 0 5)])
  (>= (+ x y z) 5))
------------------------------


test: "nested for/and forms can be flattened to a for*/and form"
------------------------------
(for/and ([x (in-range 0 5)])
  (for/and ([y (in-range 0 5)])
    (for/and ([z (in-range 0 5)])
      (<= (+ x y z) 5))))
==============================
(for*/and ([x (in-range 0 5)]
           [y (in-range 0 5)]
           [z (in-range 0 5)])
  (<= (+ x y z) 5))
------------------------------


test: "(when ...) in a for loop refactored to #:when clause"
------------------------------------------------------------
(for ([x (in-list (list 1 2 'a 3 'b 4))])
  (when (number? x)
    (displayln x)))
============================================================
(for ([x (in-list (list 1 2 'a 3 'b 4))]
      #:when (number? x))
  (displayln x))
------------------------------------------------------------


test: "(when ...) in a for* loop refactored to #:when clause"
------------------------------------------------------------
(for* ([x (in-list (list 1 2 'a 3 'b 4))])
  (when (number? x)
    (displayln x)))
============================================================
(for* ([x (in-list (list 1 2 'a 3 'b 4))]
       #:when (number? x))
  (displayln x))
------------------------------------------------------------


test: "(unless ...) in a for loop refactored to #:when clause"
------------------------------------------------------------
(for ([x (in-list (list 1 2 'a 3 'b 4))])
  (unless (number? x)
    (displayln x)))
============================================================
(for ([x (in-list (list 1 2 'a 3 'b 4))]
      #:unless (number? x))
  (displayln x))
------------------------------------------------------------


test: "(unless ...) in a for* loop refactored to #:when clause"
------------------------------------------------------------
(for* ([x (in-list (list 1 2 'a 3 'b 4))])
  (unless (number? x)
    (displayln x)))
============================================================
(for* ([x (in-list (list 1 2 'a 3 'b 4))]
       #:unless (number? x))
  (displayln x))
------------------------------------------------------------


test: "for/vector with in-range 0 n gets #:length n"
------------------------------------------------------------
(define n 5)
(for/vector ([i (in-range 0 n)]) i)
============================================================
(define n 5)
(for/vector #:length n
            ([i (in-range 0 n)])
  i)
------------------------------------------------------------


test: "for/vector with in-range n gets #:length n"
------------------------------------------------------------
(define n 5)
(for/vector ([i (in-range n)]) i)
============================================================
(define n 5)
(for/vector #:length n
            ([i (in-range n)])
  i)
------------------------------------------------------------


no-change-test: "for/vector with literal end won't have #:length added"
- (for/vector ([i (in-range 0 10)]) i)


no-change-test: "for/vector with expression end won't have #:length added"
------------------------------------------------------------
(define n 5)
(define m 3)
(for/vector ([i (in-range 0 (+ n m))]) i)
------------------------------------------------------------


no-change-test: "for/vector with non-zero start won't have #:length added"
------------------------------------------------------------
(define n 5)
(for/vector ([i (in-range 2 n)]) i)
------------------------------------------------------------


no-change-test: "for/vector with multiple clauses won't have #:length added"
------------------------------------------------------------
(define n 5)
(define m 3)
(for/vector ([i (in-range 0 n)]
             [j (in-range 0 m)]) 
  (+ i j))
------------------------------------------------------------


test: "in-hash refactorable to in-hash-keys when only the key is used"
--------------------
(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3))])
  (displayln k))
====================
(for ([k (in-hash-keys (hash 'a 1 'b 2 'c 3))])
  (displayln k))
--------------------


test: "in-hash refactorable to in-hash-values when only the value is used"
--------------------
(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3))])
  (displayln v))
====================
(for ([v (in-hash-values (hash 'a 1 'b 2 'c 3))])
  (displayln v))
--------------------


no-change-test: "in-hash not refactorable to in-hash-keys when key and value both used"
--------------------
(for ([(k v) (in-hash (hash 'a 1 'b 2 'c 3))])
  (displayln k)
  (displayln v))
--------------------


test: "in-hash between other clauses refactorable to in-hash-keys when only the key is used"
--------------------
(for ([i (in-naturals)]
      [(k v) (in-hash (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j k)))
====================
(for ([i (in-naturals)]
      [k (in-hash-keys (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j k)))
--------------------


test: "in-hash between other clauses refactorable to in-hash-values when only the value is used"
--------------------
(for ([i (in-naturals)]
      [(k v) (in-hash (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j v)))
====================
(for ([i (in-naturals)]
      [v (in-hash-values (hash 'a 1 'b 2 'c 3))]
      [j (in-naturals)])
  (displayln (list i j v)))
--------------------


test: "in-hash in for* loop refactorable to in-hash-keys"
--------------------
(for* ([(k v) (in-hash (hash 'a 1 'b 2 'c 3))])
  (displayln k))
====================
(for* ([k (in-hash-keys (hash 'a 1 'b 2 'c 3))])
  (displayln k))
--------------------


test: "in-hash in for/list loop refactorable to in-hash-keys"
--------------------
(for/list ([(k v) (in-hash (hash 'a 1 'b 2 'c 3))])
  k)
====================
(for/list ([k (in-hash-keys (hash 'a 1 'b 2 'c 3))])
  k)
--------------------


test: "in-hash in for/list loop refactorable to in-hash-values"
--------------------
(for/list ([(k v) (in-hash (hash 'a 1 'b 2 'c 3))])
  v)
====================
(for/list ([v (in-hash-values (hash 'a 1 'b 2 'c 3))])
  v)
--------------------


test: "unused in-value clause refactorable to #:do clause"
--------------------
(for* ([a (in-range 0 3)]
       [b (in-value (* a 2))]
       [c (in-range 0 a)])
  (displayln (list a c)))
====================
(for* ([a (in-range 0 3)]
       #:do [(* a 2)]
       [c (in-range 0 a)])
  (displayln (list a c)))
--------------------


no-change-test: "used in-value clause not refactorable to #:do clause"
--------------------
(for* ([a (in-range 0 3)]
       [b (in-value (* a 2))]
       [c (in-range 0 a)])
  (displayln (list a b c)))
--------------------


test: "shadowed and unused in-value clause refactorable to #:do clause"
--------------------
(for* ([a (in-range 0 3)]
       [b (in-value (* a 2))]
       [b (in-range 0 a)])
  (displayln (list a b)))
====================
(for* ([a (in-range 0 3)]
       #:do [(* a 2)]
       [b (in-range 0 a)])
  (displayln (list a b)))
--------------------
