#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out resyntax/default-recommendations/for-loop-shortcuts
               resyntax/default-recommendations/function-definition-shortcuts
               resyntax/default-recommendations/legacy-contract-migrations
               resyntax/default-recommendations/legacy-struct-migrations
               resyntax/default-recommendations/let-binding-suggestions
               resyntax/default-recommendations/list-shortcuts
               resyntax/default-recommendations/miscellaneous-suggestions)
 (contract-out
  [default-recommendations refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/default-recommendations/for-loop-shortcuts
         resyntax/default-recommendations/function-definition-shortcuts
         resyntax/default-recommendations/legacy-contract-migrations
         resyntax/default-recommendations/legacy-struct-migrations
         resyntax/default-recommendations/let-binding-suggestions
         resyntax/default-recommendations/list-shortcuts
         resyntax/default-recommendations/miscellaneous-suggestions
         resyntax/refactoring-suite)


;@----------------------------------------------------------------------------------------------------


(define default-recommendations
  (refactoring-suite
   #:name (name default-recommendations)
   #:rules
   (append (refactoring-suite-rules for-loop-shortcuts)
           (refactoring-suite-rules function-definition-shortcuts)
           (refactoring-suite-rules legacy-contract-migrations)
           (refactoring-suite-rules legacy-struct-migrations)
           (refactoring-suite-rules let-binding-suggestions)
           (refactoring-suite-rules list-shortcuts)
           (refactoring-suite-rules miscellaneous-suggestions))))
