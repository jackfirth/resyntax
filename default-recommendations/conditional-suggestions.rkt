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
  #:attributes (branches branches-size)
  #:literals (if)
  (pattern (if cond then (~or* (~var nested nested-if-else) default))
           #:with branches #`(NEWLINE [cond then]
                                      #,@(if (attribute default)
                                             #'(NEWLINE [else default])
                                             (attribute nested.branches)))
           #:attr branches-size (if (attribute default)
                                    2
                                    (+ 1 (attribute nested.branches-size)))))


(define-refactoring-rule nested-if-to-cond
  #:description "If-else chains can be converted to cond."
  #:literals (cond)
  [test:nested-if-else
   #:when (> (attribute test.branches-size) 3)
   (cond (~@ . test.branches))])


(define conditional-suggestions
  (refactoring-suite
   #:name (name conditional-suggestions)
   #:rules (list nested-if-to-cond)))
