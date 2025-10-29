#lang resyntax/test


require: resyntax/default-recommendations cond-let-replacement


header:
- #lang racket/base


test: "cond with nested let refactorable to cond with define"
------------------------------
(define (f a c)
  (cond
    [a
     (let ([x "stuff"])
       x)]
    [else c]))
==============================
(define (f a c)
  (cond
    [a
     (define x "stuff")
     x]
    [else c]))
------------------------------


test: "cond with nested let in else clause refactorable to cond with define"
------------------------------
(define (f a b)
  (cond
    [a b]
    [else
     (let ([x "stuff"])
       x)]))
==============================
(define (f a b)
  (cond
    [a b]
    [else
     (define x "stuff")
     x]))
------------------------------


test: "if clause with let in true branch refactorable to cond"
------------------------------
(define (f a b)
  (if a
      (let ([x 1])
        x)
      b))
==============================
(define (f a b)
  (cond
    [a
     (define x 1)
     x]
    [else b]))
------------------------------


test: "if clause with let in false branch refactorable to cond"
------------------------------
(define (f a b)
  (if a
      b
      (let ([x 1])
        x)))
==============================
(define (f a b)
  (cond
    [a b]
    [else
     (define x 1)
     x]))
------------------------------


test: "if clause with let in both branches refactorable to cond"
------------------------------
(define (f a)
  (if a
      (let ([x 1])
        x)
      (let ([x 1])
        x)))
==============================
(define (f a)
  (cond
    [a
     (define x 1)
     x]
    [else
     (define x 1)
     x]))
------------------------------


test: "if clause with multiline condition and let in true branch refactorable to cond"
------------------------------
(define (f a b)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      (let ([x 1])
        x)
      b))
==============================
(define (f a b)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     (define x 1)
     x]
    [else b]))
------------------------------


test: "if clause with multiline condition and let in false branch refactorable to cond"
------------------------------
(define (f a b)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      b
      (let ([x 1])
        x)))
==============================
(define (f a b)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     b]
    [else
     (define x 1)
     x]))
------------------------------


test: "if clause with multiline condition and let in both branches refactorable to cond"
------------------------------
(define (f a)
  (if (a 10000000000000000000000000000000000000
         20000000000000000000000000000000000000
         30000000000000000000000000000000000000)
      (let ([x 1])
        x)
      (let ([x 1])
        x)))
==============================
(define (f a)
  (cond
    [(a 10000000000000000000000000000000000000
        20000000000000000000000000000000000000
        30000000000000000000000000000000000000)
     (define x 1)
     x]
    [else
     (define x 1)
     x]))
------------------------------


test: "if clause with let in commented true branch refactorable to cond"
------------------------------
(define (f a b)
  (if a
      ;; This is the true case
      (let ([x 1])
        x)
      b))
==============================
(define (f a b)
  (cond
    [a
     ;; This is the true case
     (define x 1)
     x]
    [else b]))
------------------------------


test: "if clause with let in commented false branch refactorable to cond"
------------------------------
(define (f a b)
  (if a
      b
      ;; This is the false case
      (let ([x 1])
        x)))
==============================
(define (f a b)
  (cond
    [a b]
    ;; This is the false case
    [else
     (define x 1)
     x]))
------------------------------


test: "if clause with let in both commented branches refactorable to cond"
------------------------------
(define (f a)
  (if a
      ;; This is the true case
      (let ([x 1])
        x)
      ;; This is the false case
      (let ([x 1])
        x)))
==============================
(define (f a)
  (cond
    [a
     ;; This is the true case
     (define x 1)
     x]
    ;; This is the false case
    [else
     (define x 1)
     x]))
------------------------------



test: "if clause with let in true branch and commented false branch refactorable to cond"
------------------------------
(define (f a b)
  (if a
      (let ([x 1])
        x)
      ;; This is the false case
      b))
==============================
(define (f a b)
  (cond
    [a
     (define x 1)
     x]
    ;; This is the false case
    [else b]))
------------------------------


test: "if clause with let in false branch and commented true branch refactorable to cond"
------------------------------
(define (f a b)
  (if a
      ;; This is the true case
      b
      (let ([x 1])
        x)))
==============================
(define (f a b)
  (cond
    ;; This is the true case
    [a b]
    [else
     (define x 1)
     x]))
------------------------------


test: "and with let can be refactored to cond with define"
------------------------------
(and 'some-condition
     (let ([x 42])
       (* x 2)))
==============================
(cond
  ['some-condition
   (define x 42)
   (* x 2)]
  [else #f])
------------------------------


no-change-test: "and without let should not be refactored"
------------------------------
(and 'some-condition (* 42 2))
------------------------------


no-change-test: "and with empty let should not be refactored"
------------------------------
(and 'some-condition (let () (* 42 2)))
------------------------------


no-change-test: "and with more than two arguments should not be refactored"
------------------------------
(and 'some-condition 'another-condition (let ([x 42]) (* x 2)))
------------------------------

