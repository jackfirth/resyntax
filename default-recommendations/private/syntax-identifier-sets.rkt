#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [in-syntax-identifiers (-> syntax? (sequence/c identifier?))]
  [syntax-identifiers (-> syntax? (set/c identifier? #:cmp 'equal))]
  [syntax-free-identifiers (-> syntax? immutable-free-id-set?)]
  [syntax-bound-identifiers (-> syntax? immutable-bound-id-set?)]))


(require guard
         racket/sequence
         racket/set
         racket/stream
         resyntax/private/syntax-traversal
         syntax/id-set
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define (in-syntax-identifiers stx)
  (syntax-search stx
    #:datum-literals (quote)
    [(quote _) (stream)]
    [:id]))


(define (syntax-identifiers stx)
  (for/set ([id (in-syntax-identifiers stx)])
    id))


(define (syntax-free-identifiers stx)
  (immutable-free-id-set (syntax-identifiers stx)))


(define (syntax-bound-identifiers stx)
  (immutable-bound-id-set (syntax-identifiers stx)))
