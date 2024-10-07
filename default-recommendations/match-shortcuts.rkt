#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [match-shortcuts refactoring-suite?]))


(require racket/list
         racket/match
         racket/set
         resyntax/base
         resyntax/default-recommendations/private/syntax-identifier-sets
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
  #:with (new-body ...) (if (empty? (attribute body-before))
                            (attribute match-expression.as-definition-context-body)
                            #'(~focus-replacement-on
                               (match-expression.as-definition-context-body ...)))
  (body-before ... new-body ...))


(define-refactoring-suite match-shortcuts
   #:rules (single-clause-match-to-match-define))
