#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations console-io-suggestions


header:
- #lang racket/base


test: "should suggest 'any linemode with read-line when linemode not specified"
----------------------------------------
(define (foo in)
  (read-line in))
----------------------------------------
----------------------------------------
(define (foo in)
  (read-line in 'any))
----------------------------------------


test: "should suggest 'any linemode with read-line when linemode and port not specified"
----------------------------------------
(define (foo)
  (read-line))
----------------------------------------
----------------------------------------
(define (foo)
  (read-line (current-input-port) 'any))
----------------------------------------
