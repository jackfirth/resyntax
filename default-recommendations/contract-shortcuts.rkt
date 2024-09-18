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
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule nested-or/c-to-flat-or/c
  #:description "Nested `or/c` contracts can be flattened to a single, equivalent `or/c` contract."
  (~var or-tree (syntax-tree #'or/c))
  ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
  ;; any formatting between adjacent leaves.
  #:when (oneline-syntax? #'or-tree)
  #:when (>= (attribute or-tree.rank) 2)
  (or/c or-tree.leaf ...))


(define-refactoring-rule nested-and/c-to-flat-and/c
  #:description "Nested `and/c` contracts can be flattened to a single, equivalent `and/c` contract."
  (~var and-tree (syntax-tree #'and/c))
  ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
  ;; any formatting between adjacent leaves.
  #:when (oneline-syntax? #'and-tree)
  #:when (>= (attribute and-tree.rank) 2)
  (and/c and-tree.leaf ...))


(define-refactoring-rule explicit-path-string?-to-path-string?
  #:description "This contract is equivalent to the `path-string?` predicate."
  #:literals (or/c path? string?)
  (~or (or/c path? string?) (or/c string? path?))
  path-string?)


(define-refactoring-rule arrow-contract-with-rest-to-arrow-contract-with-ellipses
  #:description "This `->*` contract can be rewritten using `->` with ellipses."
  #:literals (->* listof)
  (->* (arg-contract ...)
       (~optional ())
       #:rest (~and rest-list (listof rest-contract))
       result-contract)
  (-> arg-contract ...
      rest-contract (... ...)
      result-contract))


(define-splicing-syntax-class unprotected-submodule-option
  (pattern (~optional (~seq #:unprotected-submodule submodule-name))))


(define-refactoring-rule provide/contract-to-contract-out
  #:description "The `provide/contract` form is a legacy form made obsolete by `contract-out`."
  #:literals (provide/contract)
  (provide/contract submod:unprotected-submodule-option item ...)
  (provide (contract-out (~@ . submod) item ...)))


(define-refactoring-suite contract-shortcuts
  #:rules (arrow-contract-with-rest-to-arrow-contract-with-ellipses
           explicit-path-string?-to-path-string?
           nested-or/c-to-flat-or/c
           nested-and/c-to-flat-and/c
           provide/contract-to-contract-out))
