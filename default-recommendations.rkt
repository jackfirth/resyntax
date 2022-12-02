#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out resyntax/default-recommendations/boolean-shortcuts
               resyntax/default-recommendations/comparison-shortcuts
               resyntax/default-recommendations/conditional-shortcuts
               resyntax/default-recommendations/contract-shortcuts
               resyntax/default-recommendations/for-loop-shortcuts
               resyntax/default-recommendations/function-definition-shortcuts
               resyntax/default-recommendations/hash-shortcuts
               resyntax/default-recommendations/legacy-contract-migrations
               resyntax/default-recommendations/legacy-struct-migrations
               resyntax/default-recommendations/legacy-syntax-migrations
               resyntax/default-recommendations/let-binding-suggestions
               resyntax/default-recommendations/list-shortcuts
               resyntax/default-recommendations/match-shortcuts
               resyntax/default-recommendations/miscellaneous-suggestions
               resyntax/default-recommendations/numeric-shortcuts
               resyntax/default-recommendations/syntax-shortcuts
               resyntax/default-recommendations/syntax-parse-shortcuts
               resyntax/default-recommendations/syntax-rules-shortcuts
               resyntax/default-recommendations/web-server-recommendations)
 (contract-out
  [default-recommendations refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/default-recommendations/boolean-shortcuts
         resyntax/default-recommendations/comparison-shortcuts
         resyntax/default-recommendations/conditional-shortcuts
         resyntax/default-recommendations/contract-shortcuts
         resyntax/default-recommendations/for-loop-shortcuts
         resyntax/default-recommendations/function-definition-shortcuts
         resyntax/default-recommendations/hash-shortcuts
         resyntax/default-recommendations/legacy-contract-migrations
         resyntax/default-recommendations/legacy-struct-migrations
         resyntax/default-recommendations/legacy-syntax-migrations
         resyntax/default-recommendations/let-binding-suggestions
         resyntax/default-recommendations/list-shortcuts
         resyntax/default-recommendations/match-shortcuts
         resyntax/default-recommendations/miscellaneous-suggestions
         resyntax/default-recommendations/numeric-shortcuts
         resyntax/default-recommendations/syntax-shortcuts
         resyntax/default-recommendations/syntax-parse-shortcuts
         resyntax/default-recommendations/syntax-rules-shortcuts
         resyntax/default-recommendations/web-server-recommendations
         resyntax/refactoring-suite)


;@----------------------------------------------------------------------------------------------------


(define default-recommendations
  (refactoring-suite
   #:name (name default-recommendations)
   #:rules
   (append (refactoring-suite-rules boolean-shortcuts)
           (refactoring-suite-rules comparison-shortcuts)
           (refactoring-suite-rules conditional-shortcuts)
           (refactoring-suite-rules contract-shortcuts)
           (refactoring-suite-rules for-loop-shortcuts)
           (refactoring-suite-rules function-definition-shortcuts)
           (refactoring-suite-rules hash-shortcuts)
           (refactoring-suite-rules legacy-contract-migrations)

           ;; TODO(https://github.com/jackfirth/resyntax/issues/47)
           ;; (refactoring-suite-rules legacy-struct-migrations)

           (refactoring-suite-rules legacy-syntax-migrations)
           (refactoring-suite-rules let-binding-suggestions)
           (refactoring-suite-rules list-shortcuts)
           (refactoring-suite-rules match-shortcuts)
           (refactoring-suite-rules miscellaneous-suggestions)
           (refactoring-suite-rules numeric-shortcuts)
           (refactoring-suite-rules syntax-shortcuts)
           (refactoring-suite-rules syntax-parse-shortcuts)
           (refactoring-suite-rules syntax-rules-shortcuts)
           (refactoring-suite-rules web-server-recommendations))))
