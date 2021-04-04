#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [leaves-in-syntax (->* (syntax?) ((-> syntax? boolean?)) (sequence/c syntax?))]))


(require racket/match
         racket/sequence
         racket/stream)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (leaves-in-syntax stx [leaf? flat-syntax?])
  (stream*
   (match stx
     [(? leaf?) (stream stx)]
     [(app syntax-e (and stx-list (or '() (? pair?)))) (leaves-in-syntax-pair stx-list leaf?)]
     [(app syntax-e (box substx)) (leaves-in-syntax substx leaf?)]
     [else (stream)])))


(define (leaves-in-syntax-pair stx-list [leaf? flat-syntax?])
  (stream*
   (match stx-list
     ['() (stream)]
     [(cons head '()) (leaves-in-syntax head leaf?)]
     [(cons head (? syntax? tail))
      (stream-append (leaves-in-syntax head leaf?) (leaves-in-syntax tail leaf?))]
     [(cons head (? pair? tail))
      (stream-append (leaves-in-syntax head leaf?) (leaves-in-syntax-pair tail leaf?))])))


(define (flat-syntax? stx)
  (define datum (syntax-e stx))
  (or (symbol? datum)
      (number? datum)
      (string? datum)
      (boolean? datum)
      (regexp? datum)
      (keyword? datum)))
