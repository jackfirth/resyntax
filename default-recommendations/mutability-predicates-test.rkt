#lang resyntax/test


require: resyntax/default-recommendations mutability-predicates


header:
------------------------------
#lang racket/base
(require racket/contract/base)
(define (immutable-hash? x) (and (hash? x) (immutable? x)))
(define (immutable-string? x) (and (string? x) (immutable? x)))
(define (immutable-bytes? x) (and (bytes? x) (immutable? x)))
(define (immutable-vector? x) (and (vector? x) (immutable? x)))
------------------------------


test: "hash? and immutable? can be refactored to immutable-hash?"
- (void (and/c hash? immutable?))
- (void (and/c immutable? hash?))
- (void immutable-hash?)


test: "string? and immutable? can be refactored to immutable-string?"
- (void (and/c string? immutable?))
- (void (and/c immutable? string?))
- (void immutable-string?)


test: "bytes? and immutable? can be refactored to immutable-bytes?"
- (void (and/c bytes? immutable?))
- (void (and/c immutable? bytes?))
- (void immutable-bytes?)


test: "vector? and immutable? can be refactored to immutable-vector?"
- (void (and/c vector? immutable?))
- (void (and/c immutable? vector?))
- (void immutable-vector?)


