#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [file-io-suggestions refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-suite file-io-suggestions
  #:rules ())
