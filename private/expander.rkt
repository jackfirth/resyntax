#lang racket/base


(require macro-debugger/model/debug
         racket/pretty
         rebellion/type/record)


(define-record-type expansion-history (original expanded steps))
(define-record-type expansion-step (input output local-expansions))


(define (expand-with-history stx)
  (define derivation-tree (trace stx expand))
  (expansion-history #:original (node-z1 derivation-tree)
                     #:expanded (node-z2 derivation-tree)
                     #:steps (vector derivation-tree)))


(module+ main
  (pretty-print (expand-with-history #'(module foo racket (lambda (x) (add1 x))))))
