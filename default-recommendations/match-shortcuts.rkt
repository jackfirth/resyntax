#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [match-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/match
         racket/set
         rebellion/private/static-name
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class single-clause-match-expression-with-body
  #:attributes ([refactored 1])
  #:literals (match)
  (pattern (~seq body-before ... (match subject [pattern body-after ...]))
    #:when (set-empty? (set-intersect (syntax-bound-identifiers #'(body-before ...))
                                      (syntax-bound-identifiers #'pattern)))
    #:with (refactored ...)
    #'(body-before ... (match-define pattern subject) body-after ...)))


;; This syntax class exists solely so that the single-clause-match-to-match-define rule can match two
;; cases that are shaped very differently.
(define-syntax-class expression-matching-single-clause-match-to-match-define
  #:attributes (refactored)

  (pattern (header:header-form-allowing-internal-definitions
            body:single-clause-match-expression-with-body)
    #:with refactored
    #'(header.original ... body.refactored ...))

  (pattern (branching-header:branching-form-allowing-internal-definitions-within-clauses
            clause-before ...
            (~and original-clause [clause-header body:single-clause-match-expression-with-body])
            clause-after ...)
    #:with refactored
    #'(branching-header.original ...
       clause-before ...
       (~replacement [clause-header body.refactored ...] #:original original-clause)
       clause-after ...)))


(define-refactoring-rule single-clause-match-to-match-define
  #:description "This `match` expression can be simplified using `match-define`."
  [expression:expression-matching-single-clause-match-to-match-define expression.refactored])


(define match-shortcuts
  (refactoring-suite
   #:name (name match-shortcuts)
   #:rules
   (list single-clause-match-to-match-define)))
