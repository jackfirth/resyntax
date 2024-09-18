#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-rules-shortcuts refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule define-syntax-syntax-rules-to-define-syntax-rule
  #:description
  "This `define-syntax` macro can be replaced with a simpler, equivalent `define-syntax-rule` macro."
  #:literals (define-syntax syntax-rules)
  (define-syntax macro:id (syntax-rules () [(_ . pattern) template]))
  (define-syntax-rule (macro . pattern) template))


(define-refactoring-suite syntax-rules-shortcuts
  #:rules (define-syntax-syntax-rules-to-define-syntax-rule))
