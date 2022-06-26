#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [match-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/match
         rebellion/private/static-name
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule single-clause-match-to-match-define
  #:description "This `match` expression can be simplified using `match-define`."
  #:literals (match)
  [(header:header-form-allowing-internal-definitions
    (match subject
      [pattern body ...]))
   (header.formatted ... NEWLINE
    (match-define pattern subject) NEWLINE
    (ORIGINAL-SPLICE body ...))])


(define match-shortcuts
  (refactoring-suite
   #:name (name match-shortcuts)
   #:rules
   (list single-clause-match-to-match-define)))
