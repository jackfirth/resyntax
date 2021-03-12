#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [conditional-suggestions refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/type/record
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse)


;@------------------------------------------(----------------------------------------------------------


(define-syntax-class nested-if-else
  #:attributes ([branch 1] branches-size)
  #:literals (if)

  (pattern (if cond then nested:nested-if-else)
    #:with (branch ...) #'(NEWLINE [cond then] nested.branch ...)
    #:attr branches-size (+ 1 (attribute nested.branches-size)))

  (pattern (if cond then default)
    #:with (branch ...) #'(NEWLINE [cond then] NEWLINE [else default])
    #:attr branches-size 2))


(define-refactoring-rule nested-if-to-cond
  #:description "If-else chains can be converted to cond."
  #:literals (cond)
  [nested:nested-if-else
   #:when (> (attribute nested.branches-size) 2)
   (cond nested.branch ...)])


(define conditional-suggestions
  (refactoring-suite
   #:name (name conditional-suggestions)
   #:rules (list nested-if-to-cond)))
