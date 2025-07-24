#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [mutability-predicates refactoring-suite?]))


(require racket/mutability
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule hash-and-immutable-to-immutable-hash
  #:description "This contract is equivalent to the `immutable-hash?` predicate."
  #:literals (and/c hash? immutable?)
  (~or (and/c hash? immutable?) (and/c immutable? hash?))
  immutable-hash?)


(define-refactoring-rule string-and-immutable-to-immutable-string
  #:description "This contract is equivalent to the `immutable-string?` predicate."
  #:literals (and/c string? immutable?)
  (~or (and/c string? immutable?) (and/c immutable? string?))
  immutable-string?)


(define-refactoring-rule bytes-and-immutable-to-immutable-bytes
  #:description "This contract is equivalent to the `immutable-bytes?` predicate."
  #:literals (and/c bytes? immutable?)
  (~or (and/c bytes? immutable?) (and/c immutable? bytes?))
  immutable-bytes?)


(define-refactoring-rule vector-and-immutable-to-immutable-vector
  #:description "This contract is equivalent to the `immutable-vector?` predicate."
  #:literals (and/c vector? immutable?)
  (~or (and/c vector? immutable?) (and/c immutable? vector?))
  immutable-vector?)


(define-refactoring-rule box-and-immutable-to-immutable-box
  #:description "This contract is equivalent to the `immutable-box?` predicate."
  #:literals (and/c box? immutable?)
  (~or (and/c box? immutable?) (and/c immutable? box?))
  immutable-box?)


(define-refactoring-rule hash-and-mutable-to-mutable-hash
  #:description "This contract is equivalent to the `mutable-hash?` predicate."
  #:literals (and/c hash? not/c immutable?)
  (~or (and/c hash? (not/c immutable?)) (and/c (not/c immutable?) hash?))
  mutable-hash?)


(define-refactoring-rule string-and-mutable-to-mutable-string
  #:description "This contract is equivalent to the `mutable-string?` predicate."
  #:literals (and/c string? not/c immutable?)
  (~or (and/c string? (not/c immutable?)) (and/c (not/c immutable?) string?))
  mutable-string?)


(define-refactoring-rule bytes-and-mutable-to-mutable-bytes
  #:description "This contract is equivalent to the `mutable-bytes?` predicate."
  #:literals (and/c bytes? not/c immutable?)
  (~or (and/c bytes? (not/c immutable?)) (and/c (not/c immutable?) bytes?))
  mutable-bytes?)


(define-refactoring-rule vector-and-mutable-to-mutable-vector
  #:description "This contract is equivalent to the `mutable-vector?` predicate."
  #:literals (and/c vector? not/c immutable?)
  (~or (and/c vector? (not/c immutable?)) (and/c (not/c immutable?) vector?))
  mutable-vector?)


(define-refactoring-rule box-and-mutable-to-mutable-box
  #:description "This contract is equivalent to the `mutable-box?` predicate."
  #:literals (and/c box? not/c immutable?)
  (~or (and/c box? (not/c immutable?)) (and/c (not/c immutable?) box?))
  mutable-box?)


(define-refactoring-suite mutability-predicates
  #:rules (hash-and-immutable-to-immutable-hash
           string-and-immutable-to-immutable-string
           bytes-and-immutable-to-immutable-bytes
           vector-and-immutable-to-immutable-vector
           box-and-immutable-to-immutable-box
           hash-and-mutable-to-mutable-hash
           string-and-mutable-to-mutable-string
           bytes-and-mutable-to-mutable-bytes
           vector-and-mutable-to-mutable-vector
           box-and-mutable-to-mutable-box))
