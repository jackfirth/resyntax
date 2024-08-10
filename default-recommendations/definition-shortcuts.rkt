#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/sequence
         rebellion/private/static-name
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/syntax-lines
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule define-values-values-to-define
  #:description "This use of `define-values` is unnecessary."
  #:literals (define-values values)

  [(header:header-form-allowing-internal-definitions
    (define-values (id:id ...) (values expr:expr ...))
    rest ...)
   (header.formatted ...
    (define id expr) ...
    (ORIGINAL-SPLICE rest ...))])


(define definition-shortcuts
  (refactoring-suite
   #:name (name definition-shortcuts)
   #:rules
   (list define-values-values-to-define)))
