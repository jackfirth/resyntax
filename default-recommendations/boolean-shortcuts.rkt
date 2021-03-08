#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [boolean-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/private/static-name
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule nested-or-to-flat-or
  #:description "Nested or expressions can be flattened to a single, equivalent or expression."
  [or-tree
   #:declare or-tree (syntax-tree #'or)
   ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
   ;; any formatting between adjacent leaves.
   #:when (oneline-syntax? #'or-tree)
   #:when (>= (attribute or-tree.rank) 2)
   (or or-tree.leaf ...)])


(define-refactoring-rule nested-and-to-flat-and
  #:description "Nested and expressions can be flattened to a single, equivalent and expression."
  [and-tree
   #:declare and-tree (syntax-tree #'and)
   ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
   ;; any formatting between adjacent leaves.
   #:when (oneline-syntax? #'and-tree)
   #:when (>= (attribute and-tree.rank) 2)
   (and and-tree.leaf ...)])


(define boolean-shortcuts
  (refactoring-suite
   #:name (name boolean-shortcuts)
   #:rules (list nested-and-to-flat-and nested-or-to-flat-or)))
