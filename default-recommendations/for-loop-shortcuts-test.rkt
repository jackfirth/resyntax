#lang resyntax/test


require: resyntax/default-recommendations for-loop-shortcuts


header:
- #lang racket/base


test: "map with short single-form body not refactorable"
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(define some-list (list 1 2 3))
(for/list ([x (in-list some-list)])
  (define y (* x 2))
  (+ x y))
------------------------------


test: "map with let expression to for/list with definitions"
------------------------------
(define some-list (list 1 2 3))
(map (λ (x) (let ([y 1]) (+ x y))) some-list)
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
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
------------------------------
------------------------------
(for/list ([b (in-bytes #"hello")])
  (displayln b)
  (* b 2))
------------------------------


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
(for-each (λ (x) (let ([y 1]) (displayln (+ x y)))) some-list)
------------------------------
------------------------------
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
------------------------------
------------------------------
(define some-list (list 3 5 14 10 6 5 2))
(for/and ([x (in-list some-list)])
  (and (number? x) (positive? x) (even? x) (< x 10)))
------------------------------


test: "for/and with or guarding complex expression to filter clause"
------------------------------
(define some-list (list 3 "foo" 5 14 "bar" 10 6 "baz" 5 2))
(for/and ([x (in-list some-list)])
  (or (number? x)
      (let ([l (string-length x)])
        (and (odd? l) (< l 10)))))
------------------------------
------------------------------
(define some-list (list 3 "foo" 5 14 "bar" 10 6 "baz" 5 2))
(for/and ([x (in-list some-list)]
          #:unless (number? x))
  (define l (string-length x))
  (and (odd? l) (< l 10)))
------------------------------


test: "for/and with or guarding simple expression not refactorable"
------------------------------
(define some-list (list 3 "foo" 5 14 "bar" 10 6 "baz" 5 2))
(for/and ([x (in-list some-list)])
  (or (number? x)
      (string? x)))
------------------------------


test: "for/fold building hash to for/hash"
------------------------------
(for/fold ([h (hash)])
          ([x (in-range 0 10)])
  (hash-set h x 'foo))
------------------------------
------------------------------
(for/hash ([x (in-range 0 10)])
  (values x 'foo))
------------------------------


test: "for*/fold building hash to for*/hash"
------------------------------
(for*/fold ([h (hash)])
           ([x (in-range 0 10)])
  (hash-set h x 'foo))
------------------------------
------------------------------
(for*/hash ([x (in-range 0 10)])
  (values x 'foo))
------------------------------


test: "for/fold building hash can't be refactored when referring to hash"
------------------------------
(for/fold ([h (hash)])
          ([x (in-range 0 10)])
  (displayln (hash-has-key? h x))
  (hash-set h x 'foo))
------------------------------


test: "for*/fold building hash can't be refactored when referring to hash"
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
------------------------------
------------------------------
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


test: "for/fold with conditional body refactorable to for/fold with #:when"
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (if (even? n)
        (+ accum n)
        accum)))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (if (not (even? n))
        accum
        (+ accum n))))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) (+ accum n)]
      [else accum])))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(not (even? n)) accum]
      [else (+ accum n)])))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) (+ accum n)]
      [accum])))
------------------------------
------------------------------
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
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (if (not (even? n))
        (+ accum n)
        accum)))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) accum]
      [else (+ accum n)])))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(not (even? n)) (+ accum n)]
      [else accum])))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)])
    (cond
      [(even? n) accum]
      [(+ accum n)])))
------------------------------
------------------------------
(define (foo)
  (for/fold ([accum 0])
            ([n (in-naturals)]
             #:unless (even? n))
    (+ accum n)))
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


test: "list->set with for/list to for/set"
------------------------------
(require racket/set)
(list->set
 (for/list ([x (in-range 0 10)])
   (displayln x)
   (* x x)))
------------------------------
------------------------------
(require racket/set)
(for/set ([x (in-range 0 10)])
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


test: "nested for/or forms can be flattened to a for*/or form"
------------------------------
(for/or ([x (in-range 0 5)])
  (for/or ([y (in-range 0 5)])
    (for/or ([z (in-range 0 5)])
      (>= (+ x y z) 5))))
------------------------------
------------------------------
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
------------------------------
------------------------------
(for*/and ([x (in-range 0 5)]
           [y (in-range 0 5)]
           [z (in-range 0 5)])
  (<= (+ x y z) 5))
------------------------------


test: "named let loop with conditional return over vector can be replaced by for/first"
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


test: "named let loop over list can be replaced by for/list"
------------------------------------------------------------
(require racket/list)
(let loop ([xs (list 1 2 3)])
  (cond
    [(null? xs) '()]
    [else
     (displayln (car xs))
     (cons (* (car xs) 10)
           (loop (cdr xs)))]))
------------------------------------------------------------
------------------------------------------------------------
(require racket/list)
(let loop ([xs (list 1 2 3)])
  (cond
    [(empty? xs) '()]
    [else
     (displayln (first xs))
     (cons (* (first xs) 10)
           (loop (rest xs)))]))
------------------------------------------------------------
------------------------------------------------------------
(require racket/list)
(for/list ([x (in-list (list 1 2 3))])
  (displayln x)
  (* x 10))
------------------------------------------------------------


test: "append-map with for/list can be replaced by for*/list"
------------------------------------------------------------
(require racket/list)
(append-map (λ (n)
              (for/list ([m (in-range 0 n)])
                (list n m)))
            (list 3 4 5))
------------------------------------------------------------
------------------------------------------------------------
(require racket/list)
(for*/list ([n (in-list (list 3 4 5))]
            [m (in-range 0 n)])
  (list n m))
------------------------------------------------------------


test: "append-map with multi-clause for/list can't be replaced by for*/list"
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
------------------------------------------------------------
------------------------------------------------------------
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


test: "(when ...) in a for loop refactored to #:when clause"
------------------------------------------------------------
(for ([x (in-list (list 1 2 'a 3 'b 4))])
  (when (number? x)
    (displayln x)))
------------------------------------------------------------
------------------------------------------------------------
(for ([x (in-list (list 1 2 'a 3 'b 4))]
      #:when (number? x))
  (displayln x))
------------------------------------------------------------


test: "(when ...) in a for* loop refactored to #:when clause"
------------------------------------------------------------
(for* ([x (in-list (list 1 2 'a 3 'b 4))])
  (when (number? x)
    (displayln x)))
------------------------------------------------------------
------------------------------------------------------------
(for* ([x (in-list (list 1 2 'a 3 'b 4))]
       #:when (number? x))
  (displayln x))
------------------------------------------------------------


test: "(unless ...) in a for loop refactored to #:when clause"
------------------------------------------------------------
(for ([x (in-list (list 1 2 'a 3 'b 4))])
  (unless (number? x)
    (displayln x)))
------------------------------------------------------------
------------------------------------------------------------
(for ([x (in-list (list 1 2 'a 3 'b 4))]
      #:unless (number? x))
  (displayln x))
------------------------------------------------------------


test: "(unless ...) in a for* loop refactored to #:when clause"
------------------------------------------------------------
(for* ([x (in-list (list 1 2 'a 3 'b 4))])
  (unless (number? x)
    (displayln x)))
------------------------------------------------------------
------------------------------------------------------------
(for* ([x (in-list (list 1 2 'a 3 'b 4))]
       #:unless (number? x))
  (displayln x))
------------------------------------------------------------
