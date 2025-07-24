#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutability-predicates refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule hash-and-immutable-to-immutable-hash
  #:description "This contract is equivalent to the `immutable-hash?` predicate."
  #:literals (and/c hash? immutable?)
  (~or (and/c hash? immutable?) (and/c immutable? hash?))
  #:when (identifier-binding (datum->syntax this-syntax 'immutable-hash?))
  #:with replacement-id (datum->syntax this-syntax 'immutable-hash?)
  replacement-id)


(define-refactoring-rule string-and-immutable-to-immutable-string
  #:description "This contract is equivalent to the `immutable-string?` predicate."
  #:literals (and/c string? immutable?)
  (~or (and/c string? immutable?) (and/c immutable? string?))
  #:when (identifier-binding (datum->syntax this-syntax 'immutable-string?))
  #:with replacement-id (datum->syntax this-syntax 'immutable-string?)
  replacement-id)


(define-refactoring-rule bytes-and-immutable-to-immutable-bytes
  #:description "This contract is equivalent to the `immutable-bytes?` predicate."
  #:literals (and/c bytes? immutable?)
  (~or (and/c bytes? immutable?) (and/c immutable? bytes?))
  #:when (identifier-binding (datum->syntax this-syntax 'immutable-bytes?))
  #:with replacement-id (datum->syntax this-syntax 'immutable-bytes?)
  replacement-id)


(define-refactoring-rule vector-and-immutable-to-immutable-vector
  #:description "This contract is equivalent to the `immutable-vector?` predicate."
  #:literals (and/c vector? immutable?)
  (~or (and/c vector? immutable?) (and/c immutable? vector?))
  #:when (identifier-binding (datum->syntax this-syntax 'immutable-vector?))
  #:with replacement-id (datum->syntax this-syntax 'immutable-vector?)
  replacement-id)


(define-refactoring-suite mutability-predicates
  #:rules (hash-and-immutable-to-immutable-hash
           string-and-immutable-to-immutable-string
           bytes-and-immutable-to-immutable-bytes
           vector-and-immutable-to-immutable-vector))