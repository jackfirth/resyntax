#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [contract-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule nested-or/c-to-flat-or/c
  #:description "Nested `or/c` contracts can be flattened to a single, equivalent `or/c` contract."
  [or-tree
   #:declare or-tree (syntax-tree #'or/c)
   ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
   ;; any formatting between adjacent leaves.
   #:when (oneline-syntax? #'or-tree)
   #:when (>= (attribute or-tree.rank) 2)
   (or/c or-tree.leaf ...)])


(define-refactoring-rule nested-and/c-to-flat-and/c
  #:description "Nested `and/c` contracts can be flattened to a single, equivalent `and/c` contract."
  [and-tree
   #:declare and-tree (syntax-tree #'and/c)
   ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
   ;; any formatting between adjacent leaves.
   #:when (oneline-syntax? #'and-tree)
   #:when (>= (attribute and-tree.rank) 2)
   (and/c and-tree.leaf ...)])


(define-refactoring-rule explicit-predicate/c-to-predicate/c
  #:description "This contract is equivalent to the `predicate/c` contract."
  #:literals (-> any/c boolean?)
  [(-> any/c boolean?)
   predicate/c])


(define contract-shortcuts
  (refactoring-suite
   #:name (name contract-shortcuts)
   #:rules
   (list explicit-predicate/c-to-predicate/c
         nested-or/c-to-flat-or/c
         nested-and/c-to-flat-and/c)))
