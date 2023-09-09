#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations for-loop-shortcuts


header:
- #lang racket/base


test: "for-each with short single-form body not refactorable"
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(define some-list (list 1 2 3))
(for ([x (in-list some-list)])
  (displayln x)
  (displayln x))
------------------------------


test: "for-each with let expression to for with definitions"
------------------------------
(define some-list (list 1 2 3))
(for-each (λ (x) (let ([y 1]) (displayln x))) some-list)
------------------------------
------------------------------
(define some-list (list 1 2 3))
(for ([x (in-list some-list)])
  (define y 1)
  (displayln x))
------------------------------


test: "for-each range to for"
------------------------------
(require racket/list)
(for-each
 (λ (x)
   (displayln x)
   (displayln x))
 (range 0 10))
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(for ([x (in-bytes #"hello")])
  (displayln x)
  (displayln x))
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
------------------------------
------------------------------
(define some-list (list 3 5 14 10 6 5 2))
(for/or ([x (in-list some-list)])
  (and (number? x)
       (positive? x)
       (even? x)
       (< x 10)))
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
------------------------------
------------------------------
(define some-list (list 3 5 14 10 6 5 2))
(for/and ([x (in-list some-list)])
  (and (number? x)
       (positive? x)
       (even? x)
       (< x 10)))
------------------------------


test: "for/and with or to filter clause"
------------------------------
(define some-list (list 3 5 14 10 6 5 2))
(for/and ([x (in-list some-list)])
  (or (number? x)
      (positive? x)
      (not (even? x))
      (< x 10)))
------------------------------
------------------------------
(define some-list (list 3 5 14 10 6 5 2))
(for/and ([x (in-list some-list)]
          #:unless (number? x)
          #:unless (positive? x)
          #:when (even? x))
  (< x 10))
------------------------------


test: "for/fold building hash to for/hash"
------------------------------
(for/fold ([h (hash)]) ([x (in-range 0 10)])
  (hash-set h x 'foo))
------------------------------
------------------------------
(for/hash ([x (in-range 0 10)])
  (values x 'foo))
------------------------------


test: "for*/fold building hash to for*/hash"
------------------------------
(for*/fold ([h (hash)]) ([x (in-range 0 10)])
  (hash-set h x 'foo))
------------------------------
------------------------------
(for*/hash ([x (in-range 0 10)])
  (values x 'foo))
------------------------------


test: "for/fold building hash can't be refactored when referring to hash"
------------------------------
(for/fold ([h (hash)]) ([x (in-range 0 10)])
  (displayln
   (hash-has-key? h x))
  (hash-set h x 'foo))
------------------------------


test: "for*/fold building hash can't be refactored when referring to hash"
------------------------------
(for*/fold ([h (hash)]) ([x (in-range 0 10)])
  (displayln
   (hash-has-key? h x))
  (hash-set h x 'foo))
------------------------------


test: "list->vector with for/list to for/vector"
------------------------------
(list->vector
 (for/list ([x (in-range 0 10)])
   (displayln x)
   (* x x)))
------------------------------
------------------------------
(for/vector ([x (in-range 0 10)])
  (displayln x)
  (* x x))
------------------------------


test: "nested for forms can be flattened to a for* form"
------------------------------
(for ([x (in-range 0 5)])
  (for ([y (in-range 0 5)])
    (for ([z (in-range 0 5)])
      (displayln x)
      (displayln y)
      (displayln z))))
------------------------------
------------------------------
(for* ([x (in-range 0 5)]
       [y (in-range 0 5)]
       [z (in-range 0 5)])
  (displayln x)
  (displayln y)
  (displayln z))
------------------------------


test: "non-nested for form isn't replaced by a for* form"
------------------------------
(for ([x (in-range 0 5)])
  (displayln x)
  (displayln x)
  (displayln x))
------------------------------


test: "let loop over vector can be replaced by for/first"
------------------------------------------------------------
(define vec (vector 0 1 2 3 4 5))
(let loop ([i 0])
  (and (< i (vector-length vec))
       (let ([x (vector-ref vec i)])
         (if (> x 3)
             (+ x 42)
             (loop (add1 i))))))
------------------------------------------------------------
------------------------------------------------------------
(define vec (vector 0 1 2 3 4 5))
(let loop ([i 0])
  (and (< i (vector-length vec))
       (let ([x (vector-ref vec i)])
         (if (> x 3)
             (+ x 42)
             (loop (+ i 1))))))
------------------------------------------------------------
------------------------------------------------------------
(define vec (vector 0 1 2 3 4 5))
(for/first ([x (in-vector vec)]
            #:when (> x 3))
  (+ x 42))
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
------------------------------------------------------------
------------------------------------------------------------
(require racket/list)
(define words (list 'the 'quick 'brown 'fox))
(for* ([word (in-list words)]
       [c (in-string (symbol->string word))])
  (printf "Letter: ~a\n" c)
  (printf "Letter code: ~a\n\n" (char->integer c)))
------------------------------------------------------------


test: "(apply append ...) with a for loop can be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for/list ([k (in-hash-keys formulas)])
         (hash-ref formulas k)))
------------------------------------------------------------
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(for*/list ([k (in-hash-keys formulas)]
            [v (in-list (hash-ref formulas k))])
  v)
------------------------------------------------------------


test: "(apply append ...) with a multi-clause for loop can't be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for/list ([k (in-hash-keys formulas)]
                  [i (in-naturals)])
         (hash-ref formulas k)))
------------------------------------------------------------


test: "(apply append ...) with a multi-body for loop can't be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for/list ([k (in-hash-keys formulas)])
         (displayln "formula time!")
         (hash-ref formulas k)))
------------------------------------------------------------


test: "(apply append ...) with a for* loop can be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for*/list ([k (in-hash-keys formulas)])
         (hash-ref formulas k)))
------------------------------------------------------------
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(for*/list ([k (in-hash-keys formulas)]
            [v (in-list (hash-ref formulas k))])
  v)
------------------------------------------------------------


test: "(apply append ...) with a multi-clause for* loop can be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for*/list ([k (in-hash-keys formulas)]
                  [i (in-range 0 5)])
         (hash-ref formulas k)))
------------------------------------------------------------
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(for*/list ([k (in-hash-keys formulas)]
            [i (in-range 0 5)]
            [v (in-list (hash-ref formulas k))])
  v)
------------------------------------------------------------


test: "(apply append ...) with a multi-body for* loop can't be removed"
------------------------------------------------------------
(define formulas
  (hash 'water (list 'hydrogen 'oxygen)
        'benzene (list 'hydrogen 'carbon)))
(apply append
       (for*/list ([k (in-hash-keys formulas)])
         (displayln "formula time!")
         (hash-ref formulas k)))
------------------------------------------------------------
