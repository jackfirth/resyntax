#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-source-range (-> (and/c syntax? syntax-has-position? syntax-has-span?) range?)]))


(require rebellion/base/comparator
         rebellion/base/range)


;@----------------------------------------------------------------------------------------------------


(define (syntax-has-position? stx)
  (and (syntax-position stx) #true))


(define (syntax-has-span? stx)
  (and (syntax-span stx) #true))


(define (syntax-source-range stx)
  (closed-open-range (syntax-position stx) (+ (syntax-position stx) (syntax-span stx))
                     #:comparator natural<=>))
