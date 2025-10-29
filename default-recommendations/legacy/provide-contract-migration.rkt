#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [provide-contract-migration refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class unprotected-submodule-option
  (pattern (~optional (~seq #:unprotected-submodule submodule-name))))


(define-refactoring-rule provide/contract-to-contract-out
  #:description "The `provide/contract` form is a legacy form made obsolete by `contract-out`."
  #:literals (provide/contract)
  (provide/contract submod:unprotected-submodule-option item ...)
  (provide (contract-out (~@ . submod) item ...)))


(define-refactoring-suite provide-contract-migration
  #:rules (provide/contract-to-contract-out))
