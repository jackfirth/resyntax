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
         syntax/id-set
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define (in-syntax-identifiers stx)
  (stream*
   (syntax-parse stx
     #:datum-literals (quote)
     [(quote _) (stream)]
     [(subform ...) (apply stream-append (map in-syntax-identifiers (attribute subform)))]
     [(subform ...+ . tail-form)
      (stream-append (apply stream-append (map in-syntax-identifiers (attribute subform)))
                     (in-syntax-identifiers (attribute tail-form)))]
     [id:id (stream (attribute id))]
     [_ (stream)])))


(define (syntax-identifiers stx)
  (for/set ([id (in-syntax-identifiers stx)])
    id))


(define (syntax-free-identifiers stx)
  (immutable-free-id-set (syntax-identifiers stx)))


(define (syntax-bound-identifiers stx)
  (immutable-bound-id-set (syntax-identifiers stx)))
