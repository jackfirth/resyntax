#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/definition-context
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


;; This syntax class exists solely so that the define-values-values-to-define rule can match two cases
;; that are shaped very differently.
(define-syntax-class expresion-matching-define-values-values-to-define
  #:attributes (refactored)
  #:literals (define-values values)

  (pattern (header:header-form-allowing-internal-definitions
            body-before ...
            (define-values (id:id ...) (values expr:expr ...))
            body-after ...)
    #:with refactored
    #'(header.original ...
       body-before ...
       (define id expr) ...
       body-after ...))

  (pattern (branching-header:branching-form-allowing-internal-definitions-within-clauses
            clause-before ...
            (~and original-clause
                  [clause-header
                   body-before ...
                   (define-values (id:id ...) (values expr:expr ...))
                   body-after ...])
            clause-after ...)
    #:with refactored
    #'(branching-header.original ...
       clause-before ...
       (~replacement [clause-header
                      body-before ...
                      (define id expr) ...
                      body-after ...]
                     #:original original-clause)
       clause-after ...)))


(define-refactoring-rule define-values-values-to-define
  #:description "This use of `define-values` is unnecessary."
  #:literals (define-values values)
  [expression:expresion-matching-define-values-values-to-define expression.refactored])


(define definition-shortcuts
  (refactoring-suite
   #:name (name definition-shortcuts)
   #:rules
   (list define-values-values-to-define)))
