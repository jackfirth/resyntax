#lang resyntax/test


require: resyntax/default-recommendations/keep-sorted-suggestions keep-sorted-suggestions


header:
--------------------
#lang racket/base
(require resyntax/keep-sorted)
(define apple 'apple)
(define banana 'banana)
(define mango 'mango)
(define orange 'orange)
(define zebra 'zebra)
--------------------


test: "unsorted marked list should be resorted"
------------------------------
(void (keep-sorted (list apple orange banana)))
==============================
(void (keep-sorted (list apple banana orange)))
------------------------------


test: "unsorted marked set should be resorted"
------------------------------
(require racket/set)
(void (keep-sorted (set orange apple banana)))
==============================
(require racket/set)
(void (keep-sorted (set apple banana orange)))
------------------------------


test: "unsorted marked vector should be resorted"
------------------------------
(void (keep-sorted (vector zebra apple mango)))
==============================
(void (keep-sorted (vector apple mango zebra)))
------------------------------


no-change-test: "already sorted marked list should not be changed"
------------------------------
(void (keep-sorted (list apple banana orange)))
------------------------------


no-change-test: "unmarked unsorted list should not be changed"
------------------------------
(void (list orange apple banana))
------------------------------
