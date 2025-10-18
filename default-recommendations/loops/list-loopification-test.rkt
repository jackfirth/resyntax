#lang resyntax/test
require: resyntax/default-recommendations list-loopification
header: - #lang racket/base


no-change-test: "map with short single-form body not refactorable"
------------------------------
(define some-list (list 1 2 3))
(map (λ (x) (* x 2)) some-list)
------------------------------


test: "map with long single-form body to for/list"
------------------------------
(define some-list (list 1 2 3))
(map
 (λ (a-very-very-very-long-variable-name-thats-so-very-long)
   (* a-very-very-very-long-variable-name-thats-so-very-long 2))
 some-list)
==============================
(define some-list (list 1 2 3))
(for/list ([a-very-very-very-long-variable-name-thats-so-very-long (in-list some-list)])
  (* a-very-very-very-long-variable-name-thats-so-very-long 2))
------------------------------


test: "map with multiple body forms to for/list"
------------------------------
(define some-list (list 1 2 3))
(map
 (λ (x)
   (define y (* x 2))
   (+ x y))
 some-list)
==============================
(define some-list (list 1 2 3))
(for/list ([x (in-list some-list)])
  (define y (* x 2))
  (+ x y))
------------------------------


test: "map with let expression to for/list with definitions"
------------------------------
(define some-list (list 1 2 3))
(map (λ (x) (let ([y 1]) (+ x y))) some-list)
==============================
(define some-list (list 1 2 3))
(for/list ([x (in-list some-list)])
  (define y 1)
  (+ x y))
------------------------------


test: "map range to for/list"
------------------------------
(require racket/list)
(map
 (λ (x)
   (define y 1)
   (+ x y))
 (range 0 10))
==============================
(require racket/list)
(for/list ([x (in-range 0 10)])
  (define y 1)
  (+ x y))
------------------------------


test: "map string->list to for/list in-string"
------------------------------
(map
 (λ (c)
   (displayln c)
   (char-upcase c))
 (string->list "hello"))
==============================
(for/list ([c (in-string "hello")])
  (displayln c)
  (char-upcase c))
------------------------------


test: "map bytes->list to for/list in-bytes"
------------------------------
(map
 (λ (b)
   (displayln b)
   (* b 2))
 (bytes->list #"hello"))
==============================
(for/list ([b (in-bytes #"hello")])
  (displayln b)
  (* b 2))
------------------------------


no-change-test: "for-each with short single-form body not refactorable"
------------------------------
(define some-list (list 1 2 3))
(for-each (λ (x) (displayln x)) some-list)
------------------------------


test: "for-each with long single-form body to for"
------------------------------
(define some-list (list 1 2 3))
(for-each
 (λ (a-very-very-very-long-variable-name-thats-so-very-long)
   (displayln a-very-very-very-long-variable-name-thats-so-very-long))
 some-list)
==============================
(define some-list (list 1 2 3))
(for ([a-very-very-very-long-variable-name-thats-so-very-long (in-list some-list)])
  (displayln a-very-very-very-long-variable-name-thats-so-very-long))
------------------------------


test: "for-each with multiple body forms to for"
------------------------------
(define some-list (list 1 2 3))
(for-each
 (λ (x)
   (displayln x)
   (displayln x))
 some-list)
==============================
(define some-list (list 1 2 3))
(for ([x (in-list some-list)])
  (displayln x)
  (displayln x))
------------------------------


test: "for-each with let expression to for with definitions"
------------------------------
(define some-list (list 1 2 3))
(for-each (λ (x) (let ([y 1]) (displayln (+ x y)))) some-list)
==============================
(define some-list (list 1 2 3))
(for ([x (in-list some-list)])
  (define y 1)
  (displayln (+ x y)))
------------------------------


test: "for-each range to for"
------------------------------
(require racket/list)
(for-each
 (λ (x)
   (displayln x)
   (displayln x))
 (range 0 10))
==============================
(require racket/list)
(for ([x (in-range 0 10)])
  (displayln x)
  (displayln x))
------------------------------


test: "for-each string->list to for in-string"
------------------------------
(for-each
 (λ (x)
   (displayln x)
   (displayln x))
 (string->list "hello"))
==============================
(for ([x (in-string "hello")])
  (displayln x)
  (displayln x))
------------------------------


test: "for-each bytes->list to for in-bytes"
------------------------------
(for-each
 (λ (x)
   (displayln x)
   (displayln x))
 (bytes->list #"hello"))
==============================
(for ([x (in-bytes #"hello")])
  (displayln x)
  (displayln x))
------------------------------


no-change-test: "build-list with short single-body form not refactorable"
- (build-list 10 (λ (i) (* i 2)))


test: "build-list with long single-body form refactorable to for/list"
------------------------------
(build-list 10
            (λ (a-very-very-very-long-variable-name-thats-so-very-long)
              (* a-very-very-very-long-variable-name-thats-so-very-long 2)))
==============================
(for/list ([a-very-very-very-long-variable-name-thats-so-very-long (in-range 10)])
  (* a-very-very-very-long-variable-name-thats-so-very-long 2))
------------------------------


test: "build-list with multiple body forms refactorable to for/list"
------------------------------
(build-list 10 (λ (i) (displayln i) (* i 2)))
==============================
(for/list ([i (in-range 10)])
  (displayln i)
  (* i 2))
------------------------------


test: "build-list with let expression refactorable to for/list"
------------------------------
(build-list 10 (λ (i) (let ([j (* i 2)]) (list i j))))
==============================
(for/list ([i (in-range 10)])
  (define j (* i 2))
  (list i j))
------------------------------


test: "ormap to for/or"
------------------------------
(define some-list (list 3 5 14 10 6 5 2))
(ormap
 (λ (x)
   (and (number? x)
        (positive? x)
        (even? x)
        (< x 10)))
 some-list)
==============================
(define some-list (list 3 5 14 10 6 5 2))
(for/or ([x (in-list some-list)])
  (and (number? x) (positive? x) (even? x) (< x 10)))
------------------------------


test: "andmap to for/and"
------------------------------
(define some-list (list 3 5 14 10 6 5 2))
(andmap
 (λ (x)
   (and (number? x)
        (positive? x)
        (even? x)
        (< x 10)))
 some-list)
==============================
(define some-list (list 3 5 14 10 6 5 2))
(for/and ([x (in-list some-list)])
  (and (number? x) (positive? x) (even? x) (< x 10)))
------------------------------

test: "list->vector with for/list to for/vector"
------------------------------
(list->vector
 (for/list ([x (in-range 0 10)])
   (displayln x)
   (* x x)))
==============================
(for/vector ([x (in-range 0 10)])
  (displayln x)
  (* x x))
------------------------------


test: "list->set with for/list to for/set"
------------------------------
(require racket/set)
(list->set
 (for/list ([x (in-range 0 10)])
   (displayln x)
   (* x x)))
==============================
(require racket/set)
(for/set ([x (in-range 0 10)])
  (displayln x)
  (* x x))
------------------------------


test: "append-map with for/list can be replaced by for*/list"
------------------------------------------------------------
(require racket/list)
(append-map (λ (n)
              (for/list ([m (in-range 0 n)])
                (list n m)))
            (list 3 4 5))
============================================================
(require racket/list)
(for*/list ([n (in-list (list 3 4 5))]
            [m (in-range 0 n)])
  (list n m))
------------------------------------------------------------


no-change-test: "append-map with multi-clause for/list can't be replaced by for*/list"
------------------------------------------------------------
(require racket/list)
(append-map (λ (n)
              (for/list ([m (in-range 0 n)]
                         [m2 (in-range 0 n)])
                (list n m m2)))
            (list 3 4 5))
------------------------------------------------------------


test: "append-map with multi-clause for*/list can be replaced by for*/list"
------------------------------------------------------------
(require racket/list)
(append-map (λ (n)
              (for*/list ([m (in-range 0 n)]
                          [k (in-range 0 m)])
                (list n m k)))
            (list 3 4 5))
============================================================
(require racket/list)
(for*/list ([n (in-list (list 3 4 5))]
            [m (in-range 0 n)]
            [k (in-range 0 m)])
  (list n m k))
------------------------------------------------------------


test: "for-each and append-map can be replaced by for* with #:when"
------------------------------------------------------------
(require racket/list)
(define words (list 'the 'quick 'brown 'fox))
(for-each (λ (c)
            (printf "Letter: ~a\n" c)
            (printf "Letter code: ~a\n\n" (char->integer c)))
          (append-map (λ (word) (string->list (symbol->string word)))
                      words))
============================================================
(require racket/list)
(define words (list 'the 'quick 'brown 'fox))
(for* ([word (in-list words)]
       [c (in-string (symbol->string word))])
  (printf "Letter: ~a\n" c)
  (printf "Letter code: ~a\n\n" (char->integer c)))
------------------------------------------------------------


no-change-test: "(apply append ...) with a multi-clause for loop can't be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for/list ([k (in-hash-keys formulas)]
                  [i (in-naturals)])
         (hash-ref formulas k)))
------------------------------------------------------------


no-change-test: "(apply append ...) with a multi-body for loop can't be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for/list ([k (in-hash-keys formulas)])
         (displayln "formula time!")
         (hash-ref formulas k)))
------------------------------------------------------------


no-change-test: "(apply append ...) with a multi-body for* loop can't be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for*/list ([k (in-hash-keys formulas)])
         (displayln "formula time!")
         (hash-ref formulas k)))
------------------------------------------------------------


test: "index-mutating map to for/list with index"
--------------------
(require racket/list)
(let ([i 0])
  (map (λ (x)
         (set! i (+ i 1))
         (list i x))
       (range 10)))
====================
(require racket/list)
(for/list ([x (in-range 10)]
           [i (in-naturals 1)])
  (list i x))
--------------------
