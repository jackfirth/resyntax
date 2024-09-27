#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [match-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/match
         racket/set
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class single-clause-match
  #:literals (match)
  #:attributes (match-pattern [as-definition-context-body 1])

  (pattern (match subject [match-pattern body ...])
    #:with definition #'(match-define match-pattern subject)
    #:with (as-definition-context-body ...)
    #`(~splicing-replacement (definition body ...) #:original #,this-syntax)))


(define-definition-context-refactoring-rule single-clause-match-to-match-define
  #:description "This `match` expression can be simplified using `match-define`."
  #:literals (match)
  (~seq body-before ... match-expression:single-clause-match)
  #:when (set-empty? (set-intersect (syntax-bound-identifiers #'(body-before ...))
                                    (syntax-bound-identifiers #'match-expression.match-pattern)))
  (body-before ... (~@ . (~focus-replacement-on (match-expression.as-definition-context-body ...)))))


(define-refactoring-suite match-shortcuts
   #:rules (single-clause-match-to-match-define))
