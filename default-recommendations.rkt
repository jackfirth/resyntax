#lang racket/base

(require racket/contract/base)


(provide
 (contract-out
  [default-recommendations (listof refactoring-rule?)]))


(require resyntax/refactoring-rule
         resyntax/default-recommendations/for-loop-shortcuts
         resyntax/default-recommendations/legacy-contract-migrations
         resyntax/default-recommendations/let-binding-suggestions
         resyntax/default-recommendations/miscellaneous-suggestions)


;@----------------------------------------------------------------------------------------------------


(define default-recommendations
  (append for-loop-shortcuts
          legacy-contract-migrations
          let-binding-suggestions
          miscellaneous-suggestions))
