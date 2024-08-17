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
         syntax/id-set)


;@----------------------------------------------------------------------------------------------------


(define (in-syntax-identifiers stx)
  (stream*
   (guarded-block
     (guard (not (identifier? stx)) #:else (stream stx))
     (guard-match (or (cons head tail) (? syntax? (app syntax-e (cons head tail)))) stx #:else
       (stream))
     (stream-append (in-syntax-identifiers head) (in-syntax-identifiers tail)))))


(define (syntax-identifiers stx)
  (for/set ([id (in-syntax-identifiers stx)])
    id))


(define (syntax-free-identifiers stx)
  (immutable-free-id-set (syntax-identifiers stx)))


(define (syntax-bound-identifiers stx)
  (immutable-bound-id-set (syntax-identifiers stx)))
