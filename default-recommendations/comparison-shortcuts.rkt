#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [comparison-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class comparison-of-difference-and-zero
  #:attributes (direct-comparison)
  #:literals (- > < >= <=)
  (pattern (> (- x y) 0) #:with direct-comparison #'(> x y))
  (pattern (< (- x y) 0) #:with direct-comparison #'(< x y))
  (pattern (>= (- x y) 0) #:with direct-comparison #'(>= x y))
  (pattern (<= (- x y) 0) #:with direct-comparison #'(<= x y))
  (pattern (> 0 (- x y)) #:with direct-comparison #'(<= x y))
  (pattern (< 0 (- x y)) #:with direct-comparison #'(>= x y))
  (pattern (>= 0 (- x y)) #:with direct-comparison #'(< x y))
  (pattern (<= 0 (- x y)) #:with direct-comparison #'(> x y)))


(define-refactoring-rule comparison-of-difference-and-zero-to-direct-comparison
  #:description "This comparison can be replaced with a simpler, more direct comparison."
  comparison:comparison-of-difference-and-zero
  comparison.direct-comparison)


(define-syntax-class two-exclusive-comparisons
  #:attributes (x lower-bound upper-bound)
  #:literals (and > <)

  (pattern (and (< x:id upper-bound:expr) (> x2:id lower-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (> x:id lower-bound:expr) (< x2:id upper-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (< x:id upper-bound:expr) (< lower-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (< lower-bound:expr x:id) (< x2:id upper-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (> upper-bound:expr x:id) (> x2:id lower-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (> x:id lower-bound:expr) (> upper-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (> upper-bound:expr x:id) (< lower-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (< lower-bound:expr x:id) (> upper-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2)))


(define-syntax-class two-inclusive-comparisons
  #:attributes (x lower-bound upper-bound)
  #:literals (and >= <=)

  (pattern (and (<= x:id upper-bound:expr) (>= x2:id lower-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (>= x:id lower-bound:expr) (<= x2:id upper-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (<= x:id upper-bound:expr) (<= lower-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (<= lower-bound:expr x:id) (<= x2:id upper-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (>= upper-bound:expr x:id) (>= x2:id lower-bound:expr))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (>= x:id lower-bound:expr) (>= upper-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (>= upper-bound:expr x:id) (<= lower-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2))

  (pattern (and (<= lower-bound:expr x:id) (>= upper-bound:expr x2:id))
    #:when (free-identifier=? #'x #'x2)))


(define-refactoring-rule two-exclusive-comparisons-to-triple-comparison
  #:description
  "Comparison functions like `<` accept multiple arguments, so this condition can be simplified."
  comparison:two-exclusive-comparisons
  (< comparison.lower-bound comparison.x comparison.upper-bound))


(define-refactoring-rule two-inclusive-comparisons-to-triple-comparison
  #:description
  "Comparison functions like `<=` accept multiple arguments, so this condition can be simplified."
  comparison:two-inclusive-comparisons
  (<= comparison.lower-bound comparison.x comparison.upper-bound))


(define-refactoring-suite comparison-shortcuts
  #:rules (comparison-of-difference-and-zero-to-direct-comparison
           two-exclusive-comparisons-to-triple-comparison
           two-inclusive-comparisons-to-triple-comparison))
