#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-parse-shortcuts refactoring-suite?]))


(require resyntax/base)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-suite syntax-parse-shortcuts
  #:rules ())
