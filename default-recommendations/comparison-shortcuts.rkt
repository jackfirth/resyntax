#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [comparison-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/private/static-name
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
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
  [comparison:comparison-of-difference-and-zero
   comparison.direct-comparison])


(define comparison-shortcuts
  (refactoring-suite
   #:name (name comparison-shortcuts)
   #:rules (list comparison-of-difference-and-zero-to-direct-comparison)))
