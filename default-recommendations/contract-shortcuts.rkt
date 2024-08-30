#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [contract-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
         resyntax/private/syntax-replacement
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


(define-refactoring-rule explicit-path-string?-to-path-string?
  #:description "This contract is equivalent to the `path-string?` predicate."
  #:literals (or/c path? string?)
  [(~or (or/c path? string?) (or/c string? path?)) path-string?])


(define-refactoring-rule arrow-contract-with-rest-to-arrow-contract-with-ellipses
  #:description "This `->*` contract can be rewritten using `->` with ellipses."
  #:literals (->* listof)
  [((~and arrow-id ->*)
    (~and args (arg-contract ...))
    (~optional ())
    (~and rest-kw #:rest) (~and rest-list (listof rest-contract))
    result-contract)
   (-> (ORIGINAL-GAP arrow-id args)
       arg-contract ...
       rest-contract (... ...)
       (ORIGINAL-GAP rest-list result-contract)
       result-contract)])


(define contract-shortcuts
  (refactoring-suite
   #:name (name contract-shortcuts)
   #:rules
   (list arrow-contract-with-rest-to-arrow-contract-with-ellipses
         explicit-path-string?-to-path-string?
         nested-or/c-to-flat-or/c
         nested-and/c-to-flat-and/c)))
