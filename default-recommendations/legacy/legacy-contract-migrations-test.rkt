#lang resyntax/test


require: resyntax/default-recommendations legacy-contract-migrations


header:
------------------------------
#lang racket/base
(require racket/contract)
------------------------------


test: "predicate/c in expression position refactorable to ->"
- (void predicate/c)
- (void (-> any/c boolean?))


test: "predicate/c in contract-out refactorable to ->"
------------------------------
(provide (contract-out [foo? predicate/c]))
(define (foo? _)
  #true)
==============================
(provide (contract-out [foo? (-> any/c boolean?)]))
(define (foo? _)
  #true)
------------------------------


test: "predicate/c in define/contract refactorable to ->"
------------------------------
(require racket/contract/region)
(define/contract (foo? _)
  predicate/c
  #true)
==============================
(require racket/contract/region)
(define/contract (foo? _)
  (-> any/c boolean?)
  #true)
------------------------------


test: "false/c refactorable to #f"
- (void false/c)
- (void #f)


test: "false/c in contract-out refactorable to #f"
------------------------------
(provide (contract-out [some-val false/c]))
(define some-val #f)
==============================
(provide (contract-out [some-val #f]))
(define some-val #f)
------------------------------


test: "symbols refactorable to or/c"
- (void (symbols 'a 'b 'c))
- (void (or/c 'a 'b 'c))


test: "symbols with single symbol refactorable to or/c"
- (void (symbols 'foo))
- (void (or/c 'foo))


test: "vector-immutableof refactorable to vectorof with #:immutable"
- (void (vector-immutableof string?))
- (void (vectorof string? #:immutable #t))


test: "vector-immutable/c refactorable to vector/c with #:immutable"
- (void (vector-immutable/c string? number?))
- (void (vector/c string? number? #:immutable #t))


test: "vector-immutable/c with single contract refactorable to vector/c with #:immutable"
- (void (vector-immutable/c string?))
- (void (vector/c string? #:immutable #t))


test: "box-immutable/c refactorable to box/c with #:immutable"
- (void (box-immutable/c string?))
- (void (box/c string? #:immutable #t))


test: "flat-contract refactorable to predicate"
- (void (flat-contract string?))
- (void string?)


test: "flat-contract in contract-out refactorable to predicate"
------------------------------
(provide (contract-out [some-val (flat-contract string?)]))
(define some-val "hello")
==============================
(provide (contract-out [some-val string?]))
(define some-val "hello")
------------------------------


test: "contract-struct refactorable to struct"
- (contract-struct person (name age))
- (struct person (name age))


test: "define-contract-struct refactorable to struct with extra constructor"
- (define-contract-struct point (x y))
- (struct point (x y) #:extra-constructor-name make-point)
