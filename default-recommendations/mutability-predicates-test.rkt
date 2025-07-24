#lang resyntax/test


require: resyntax/default-recommendations mutability-predicates


header:
------------------------------
#lang racket/base
(require racket/contract/base
         racket/mutability)
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


test: "box? and immutable? can be refactored to immutable-box?"
- (void (and/c box? immutable?))
- (void (and/c immutable? box?))
- (void immutable-box?)


test: "hash? and (not/c immutable?) can be refactored to mutable-hash?"
- (void (and/c hash? (not/c immutable?)))
- (void (and/c (not/c immutable?) hash?))
- (void mutable-hash?)


test: "string? and (not/c immutable?) can be refactored to mutable-string?"
- (void (and/c string? (not/c immutable?)))
- (void (and/c (not/c immutable?) string?))
- (void mutable-string?)


test: "bytes? and (not/c immutable?) can be refactored to mutable-bytes?"
- (void (and/c bytes? (not/c immutable?)))
- (void (and/c (not/c immutable?) bytes?))
- (void mutable-bytes?)


test: "vector? and (not/c immutable?) can be refactored to mutable-vector?"
- (void (and/c vector? (not/c immutable?)))
- (void (and/c (not/c immutable?) vector?))
- (void mutable-vector?)


test: "box? and (not/c immutable?) can be refactored to mutable-box?"
- (void (and/c box? (not/c immutable?)))
- (void (and/c (not/c immutable?) box?))
- (void mutable-box?)


