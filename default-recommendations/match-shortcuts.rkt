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


(define-definition-context-refactoring-rule single-clause-match-to-match-define
  #:description "This `match` expression can be simplified using `match-define`."
  #:literals (match)
  (~seq body-before ... (match subject [pattern body-after ...]))
  #:when (set-empty? (set-intersect (syntax-bound-identifiers #'(body-before ...))
                                    (syntax-bound-identifiers #'pattern)))
  (body-before ... (match-define pattern subject) body-after ...))


(define match-shortcuts
  (refactoring-suite
   #:name (name match-shortcuts)
   #:rules
   (list single-clause-match-to-match-define)))
