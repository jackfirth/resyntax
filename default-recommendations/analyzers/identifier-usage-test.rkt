#lang resyntax/test


require: resyntax/default-recommendations/analyzers/identifier-usage identifier-usage-analyzer
header: - #lang racket/base


analysis-test: "unused module-level variable"
- (define a 1)
@inspect - a
@property usage-count
@assert 0


analysis-test: "once-used module-level variable"
--------------------
(define a 1)
(void a)
--------------------
@within - (define a 1)
@inspect - a
@property usage-count
@assert 1


analysis-test: "thrice-used module-level variable"
--------------------
(define a 1)
(void a a a)
--------------------
@within - (define a 1)
@inspect - a
@property usage-count
@assert 3


analysis-test: "unused let-bound variable"
--------------------
(let ([a 1])
  (void))
--------------------
@inspect - a
@property usage-count
@assert 0


analysis-test: "once-used let-bound variable"
--------------------
(let ([a 1])
  (void a))
--------------------
@within - [a 1]
@inspect - a
@property usage-count
@assert 1


analysis-test: "thrice-used let-bound variable"
--------------------
(let ([a 1])
  (void a a a))
--------------------
@within - [a 1]
@inspect - a
@property usage-count
@assert 3


analysis-test: "unused module-level function definition"
- (define (f x) x)
@inspect - f
@property usage-count
@assert 0


analysis-test: "once-used module-level function definition"
--------------------
(define (f x) x)
(f 1)
--------------------
@within - (f x)
@inspect - f
@property usage-count
@assert 1


analysis-test: "twice-used module-level function definition"
--------------------
(define (f x) x)
(void (f 1) (f 2))
--------------------
@within - (f x)
@inspect - f
@property usage-count
@assert 2


analysis-test: "unused positional argument in module-level function"
--------------------
(define (f x y) x)
(f 1 2)
--------------------
@within - (define (f x y) x)
@inspect - y
@property usage-count
@assert 0


analysis-test: "once-used positional argument in module-level function"
--------------------
(define (f x y) y)
(f 1 2)
--------------------
@within - (define (f x y) y)
@inspect - y
@property usage-count
@assert 1


analysis-test: "twice-used positional argument in module-level function"
--------------------
(define (f x y) (void y y))
(f 1 2)
--------------------
@within - (define (f x y) (void y y))
@inspect - y
@property usage-count
@assert 2


analysis-test: "unused keyword argument in module-level function"
--------------------
(define (f x #:key key) x)
(f 1 #:key 2)
--------------------
@within - (define (f x #:key key) x)
@inspect - key
@property usage-count
@assert 0


analysis-test: "once-used keyword argument in module-level function"
--------------------
(define (f x #:key key) key)
(f 1 #:key 2)
--------------------
@within - (define (f x #:key key) key)
@inspect - key
@property usage-count
@assert 1


analysis-test: "unused rest argument in module-level function"
--------------------
(define (f x . rest) x)
(f 1 2 3)
--------------------
@within - (define (f x . rest) x)
@inspect - rest
@property usage-count
@assert 0


analysis-test: "once-used rest argument in module-level function"
--------------------
(define (f x . rest) rest)
(f 1 2 3)
--------------------
@within - (define (f x . rest) rest)
@inspect - rest
@property usage-count
@assert 1


analysis-test: "unused positional argument in lambda"
- ((lambda (x y) x) 1 2)
@within - (lambda (x y) x)
@inspect - y
@property usage-count
@assert 0


analysis-test: "once-used positional argument in lambda"
- ((lambda (x y) y) 1 2)
@within - (lambda (x y) y)
@inspect - y
@property usage-count
@assert 1


analysis-test: "twice-used positional argument in lambda"
- ((lambda (x y) (void y y)) 1 2)
@within - (lambda (x y) (void y y))
@inspect - y
@property usage-count
@assert 2


analysis-test: "unused keyword argument in lambda"
- ((lambda (x #:key key) x) 1 #:key 2)
@within - (lambda (x #:key key) x)
@inspect - key
@property usage-count
@assert 0


analysis-test: "once-used keyword argument in lambda"
- ((lambda (x #:key key) key) 1 #:key 2)
@within - (lambda (x #:key key) key)
@inspect - key
@property usage-count
@assert 1


analysis-test: "unused rest argument in lambda"
- ((lambda (x . rest) x) 1 2 3)
@within - (lambda (x . rest) x)
@inspect - rest
@property usage-count
@assert 0


analysis-test: "once-used rest argument in lambda"
- ((lambda (x . rest) rest) 1 2 3)
@within - (lambda (x . rest) rest)
@inspect - rest
@property usage-count
@assert 1


analysis-test: "unused module-level macro definition"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  #'(void))
--------------------
@inspect - m
@property usage-count
@assert 0


analysis-test: "unused syntax argument in macro definition"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  #'(void))
--------------------
@inspect - stx
@property usage-count
@assert 0


analysis-test: "once-used syntax argument in macro definition"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  stx)
--------------------
@inspect - stx
@property usage-count
@assert 1


analysis-test: "unused internal variable definition in function"
--------------------
(define (f)
  (define x 1)
  (void))
(f)
--------------------
@within - (define x 1)
@inspect - x
@property usage-count
@assert 0


analysis-test: "once-used internal variable definition in function"
--------------------
(define (f)
  (define x 1)
  x)
(f)
--------------------
@within - (define x 1)
@inspect - x
@property usage-count
@assert 1


analysis-test: "twice-used internal variable definition in function"
--------------------
(define (f)
  (define x 1)
  (void x x))
(f)
--------------------
@within - (define x 1)
@inspect - x
@property usage-count
@assert 2


analysis-test: "unused internal function definition in function"
--------------------
(define (f)
  (define (g y) y)
  (void))
(f)
--------------------
@within - (define (g y) y)
@inspect - g
@property usage-count
@assert 0


analysis-test: "once-used internal function definition in function"
--------------------
(define (f)
  (define (g y) y)
  (g 1))
(f)
--------------------
@within - (define (g y) y)
@inspect - g
@property usage-count
@assert 1


analysis-test: "twice-used internal function definition in function"
--------------------
(define (f)
  (define (g y) y)
  (void (g 1) (g 2)))
(f)
--------------------
@within - (define (g y) y)
@inspect - g
@property usage-count
@assert 2


analysis-test: "unused local variable in macro definition"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  (define a 1)
  stx)
--------------------
@inspect - a
@property usage-count
@assert 0


analysis-test: "once-used local variable in macro definition"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  (define a 1)
  (void a)
  stx)
--------------------
@within - (define a 1)
@inspect - a
@property usage-count
@assert 1


analysis-test: "twice-used local variable in macro definition"
--------------------
(require (for-syntax racket/base))
(define-syntax (m stx)
  (define a 1)
  (void a a)
  stx)
--------------------
@within - (define a 1)
@inspect - a
@property usage-count
@assert 2
