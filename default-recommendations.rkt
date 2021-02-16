#lang racket/base

(require racket/contract/base)


(provide
 (contract-out
  [default-recommendations refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/default-recommendations/for-loop-shortcuts
         resyntax/default-recommendations/legacy-contract-migrations
         resyntax/default-recommendations/legacy-struct-migrations
         resyntax/default-recommendations/let-binding-suggestions
         resyntax/default-recommendations/miscellaneous-suggestions
         resyntax/refactoring-rule
         resyntax/refactoring-suite)


;@----------------------------------------------------------------------------------------------------


(define default-recommendations
  (refactoring-suite
   #:name (name default-recommendations)
   #:rules
   (append (refactoring-suite-rules for-loop-shortcuts)
           (refactoring-suite-rules legacy-contract-migrations)
           (refactoring-suite-rules legacy-struct-migrations)
           (refactoring-suite-rules let-binding-suggestions)
           (refactoring-suite-rules miscellaneous-suggestions))))
