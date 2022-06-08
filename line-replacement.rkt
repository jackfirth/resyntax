#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [line-replacement
   (-> #:start-line exact-positive-integer?
       #:original-lines (sequence/c string?)
       #:new-lines (sequence/c string?)
       line-replacement?)]
  [line-replacement? predicate/c]
  [line-replacement-start-line (-> line-replacement? exact-positive-integer?)]
  [line-replacement-original-end-line (-> line-replacement? exact-positive-integer?)]
  [line-replacement-original-lines
   (-> line-replacement? (vectorof (and/c string? immutable?) #:immutable #true #:flat? #true))]
  [line-replacement-new-end-line (-> line-replacement? exact-positive-integer?)]
  [line-replacement-new-lines
   (-> line-replacement? (vectorof (and/c string? immutable?) #:immutable #true #:flat? #true))]
  [line-replacement-new-text (-> line-replacement? (and/c string? immutable?))]))


(require racket/sequence
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record)


;@----------------------------------------------------------------------------------------------------


(define-record-type line-replacement (start-line original-lines new-lines)
  #:omit-root-binding)


(define (line-replacement #:start-line start-line
                          #:original-lines original-lines
                          #:new-lines new-lines)
  (define immutable-original-lines
    (for/vector ([line original-lines])
      (string->immutable-string line)))
  (define immutable-new-lines
    (for/vector ([line new-lines])
      (string->immutable-string line)))
  (constructor:line-replacement #:start-line start-line
                                #:original-lines immutable-original-lines
                                #:new-lines immutable-new-lines))


(define (line-replacement-original-end-line replacement)
  (+ (line-replacement-start-line replacement)
     (vector-length (line-replacement-original-lines replacement))))


(define (line-replacement-new-end-line replacement)
  (+ (line-replacement-start-line replacement)
     (vector-length (line-replacement-new-lines replacement))))


(define (line-replacement-new-text replacement)
  (transduce (line-replacement-new-lines replacement) #:into (join-into-string "\n")))
