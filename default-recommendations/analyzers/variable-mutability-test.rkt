#lang resyntax/test
require: resyntax/default-recommendations default-recommendations
header: - #lang racket/base


analysis-test: "unassigned module binding marked immutable"
- (define a 1)
@inspect - a
@property variable-mutability
@assert immutable


analysis-test: "assigned module binding marked mutable"
--------------------
(define a 1)
(set! a 2)
--------------------
@within - (define a 1)
@inspect - a
@property variable-mutability
@assert mutable


analysis-test: "unassigned phase 1 module binding marked immutable"
--------------------
(require (for-syntax racket/base))
(begin-for-syntax
  (define a 1))
--------------------
@inspect - a
@property variable-mutability
@assert immutable


analysis-test: "assigned phase 1 module binding marked mutable"
--------------------
(require (for-syntax racket/base))
(begin-for-syntax
  (define a 1)
  (set! a 2))
--------------------
@within - (define a 1)
@inspect - a
@property variable-mutability
@assert mutable


analysis-test: "unassigned phase 2 module binding marked immutable"
--------------------
(require (for-syntax racket/base)
         (for-meta 2 racket/base))
(begin-for-syntax
  (begin-for-syntax
    (define a 1)))
--------------------
@inspect - a
@property variable-mutability
@assert immutable


analysis-test: "assigned phase 2 module binding marked mutable"
--------------------
(require (for-syntax racket/base)
         (for-meta 2 racket/base))
(begin-for-syntax
  (begin-for-syntax
    (define a 1)
    (set! a 2)))
--------------------
@within - (define a 1)
@inspect - a
@property variable-mutability
@assert mutable


analysis-test: "function definition marked immutable"
--------------------
(define (f)
  (void))
--------------------
@inspect - f
@property variable-mutability
@assert immutable


analysis-test: "reassigned function definition marked mutable"
--------------------
(define (f)
  (void))
(set! f (λ () (displayln "hi")))
--------------------
@within - (f)
@inspect - f
@property variable-mutability
@assert mutable


analysis-test: "function argument marked immutable"
--------------------
(define (f x)
  (void))
--------------------
@inspect - x
@property variable-mutability
@assert immutable


analysis-test: "reassigned function argument marked mutable"
--------------------
(define (f x)
  (set! x 42)
  (void))
--------------------
@within - (f x)
@inspect - x
@property variable-mutability
@assert mutable


analysis-test: "macro definition marked immutable"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  #'(void))
--------------------
@inspect - m
@property variable-mutability
@assert immutable


analysis-test: "macro definition's syntax argument marked immutable"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  #'(void))
--------------------
@inspect - stx
@property variable-mutability
@assert immutable


analysis-test: "macro definition's syntax argument marked mutable when reassigned"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  (set! stx 42)
  #'(void))
--------------------
@inspect - stx
@property variable-mutability
@assert mutable


analysis-test: "module binding marked mutable when reassigned via macro"
--------------------
(require (for-syntax racket/base))
(define a 1)
(define-syntax (m stx)
  #'(set! a 2))
(m)
--------------------
@within - (define a 1)
@inspect - a
@property variable-mutability
@assert mutable


analysis-test: "module binding marked immutable when reassigning macro is unused"
--------------------
(require (for-syntax racket/base))
(define a 1)
(define-syntax (m stx)
  #'(set! a 2))
--------------------
@within - (define a 1)
@inspect - a
@property variable-mutability
@assert immutable
