#lang resyntax/test


require: resyntax/default-recommendations named-let-loopification


header:
- #lang racket/base


test: "named let loop over counter can be replaced by for in-range"
------------------------------------------------------------
(define (f x a b dx)
  (let loop ([x a])
    (when (< x b)
      (displayln x)
      (loop (+ x dx)))))
============================================================
(define (f x a b dx)
  (for ([x (in-range a b dx)])
    (displayln x)))
------------------------------------------------------------


no-change-test: "named let loop over counter not refactorable when loop function used elsewhere"
------------------------------------------------------------
(define (f x a b dx)
  (let loop ([x a])
    (when (< x b)
      (displayln x)
      (displayln loop)
      (loop (+ x dx)))))
------------------------------------------------------------


test: "named let loop can be refactored to for/first with in-naturals"
------------------------------------------------------------
(define (f c g)
  (let loop ([i 0])
    (cond
      [(c i)
       (loop (+ i 1))]
      [else
       (g i)])))
============================================================
(define (f c g)
  (for/first ([i (in-naturals 0)]
              #:unless (c i))
    (g i)))
------------------------------------------------------------


no-change-test: "named let loop not refactorable to for/first with in-naturals when loop referenced"
------------------------------------------------------------
(define (f c g)
  (let loop ([i 0])
    (cond
      [(c i)
       (loop (+ i 1))]
      [else
       (displayln loop)
       (g i)])))
------------------------------------------------------------


test: "named let loop with conditional return over vector can be replaced by for/first"
------------------------------------------------------------
(define vec (vector 0 1 2 3 4 5))
(let loop ([i 0])
  (and (< i (vector-length vec))
       (let ([x (vector-ref vec i)])
         (if (> x 3)
             (+ x 42)
             (loop (add1 i))))))
============================================================
(define vec (vector 0 1 2 3 4 5))
(let loop ([i 0])
  (and (< i (vector-length vec))
       (let ([x (vector-ref vec i)])
         (if (> x 3)
             (+ x 42)
             (loop (+ i 1))))))
============================================================
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
============================================================
(require racket/list)
(let loop ([xs (list 1 2 3)])
  (cond
    [(empty? xs) '()]
    [else
     (displayln (first xs))
     (cons (* (first xs) 10)
           (loop (rest xs)))]))
============================================================
(require racket/list)
(for/list ([x (in-list (list 1 2 3))])
  (displayln x)
  (* x 10))
------------------------------------------------------------


test: "named let loop can be replaced with for/and when equivalent"
------------------------------------------------------------
(require racket/list)
(define (f xs big? red?)
  (let loop ([xs xs])
    (cond
      [(empty? xs) #true]
      [(and (big? (first xs)) (not (red? (car xs))))
       (loop (rest xs))]
      [else #false])))
============================================================
(require racket/list)
(define (f xs big? red?)
  (for/and ([x (in-list xs)])
    (and (big? x) (not (red? x)))))
------------------------------------------------------------


test: "named let loop can be replaced with for/or when equivalent"
------------------------------------------------------------
(require racket/list)
(define (f xs big? red?)
  (let loop ([xs xs])
    (cond
      [(empty? xs) #false]
      [(and (big? (first xs)) (not (red? (car xs)))) #true]
      [else (loop (rest xs))])))
============================================================
(require racket/list)
(define (f xs big? red?)
  (for/or ([x (in-list xs)])
    (and (big? x) (not (red? x)))))
------------------------------------------------------------


test: "read-until-eof loop refactorable to for loop with in-port"
------------------------------------------------------------
(define (print-reads)
  (let loop ([v (read)])
    (unless (eof-object? v)
      (displayln v)
      (loop (read)))))
============================================================
(define (print-reads)
  (for ([v (in-port)])
    (displayln v)))
------------------------------------------------------------
