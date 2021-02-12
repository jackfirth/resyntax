#lang racket/base

(require racket/contract/base)


(provide
 (contract-out
  [default-recommendations (listof refactoring-rule?)]))


(require resyntax/default-recommendations/for-loop-shortcuts
         resyntax/default-recommendations/legacy-contract-migrations
         resyntax/default-recommendations/legacy-struct-migrations
         resyntax/default-recommendations/let-binding-suggestions
         resyntax/default-recommendations/miscellaneous-suggestions
         resyntax/refactoring-rule)


;@----------------------------------------------------------------------------------------------------


(define default-recommendations
  (append for-loop-shortcuts
          legacy-contract-migrations
          legacy-struct-migrations
          let-binding-suggestions
          miscellaneous-suggestions))
