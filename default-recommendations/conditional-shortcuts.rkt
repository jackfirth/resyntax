#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [conditional-shortcuts refactoring-suite?]))


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
  #:description "This if-else chain can be converted to a cond expression."
  #:literals (cond)
  [nested:nested-if-else
   #:when (> (attribute nested.branches-size) 2)
   (cond nested.branch ...)])


(define-refactoring-rule if-else-false-to-and
  #:description "This conditional expression can be replaced with a simpler, equivalent expression."
  #:literals (if)
  [(if condition then-branch #false)
   (and (ORIGINAL-SPLICE condition then-branch))])


(define-refactoring-rule if-x-else-x-to-and
  #:description "This conditional expression can be replaced with a simpler, equivalent expression."
  #:literals (if)
  [(if x:id then-branch:expr y:id)
   #:when (free-identifier=? #'x #'y)
   (and (ORIGINAL-SPLICE x then-branch))])


(define conditional-shortcuts
  (refactoring-suite
   #:name (name conditional-shortcuts)
   #:rules
   (list if-else-false-to-and
         if-x-else-x-to-and
         nested-if-to-cond)))
