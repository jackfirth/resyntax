#lang racket/base


(require racket/contract/base)


(provide
 (all-from-out resyntax/default-recommendations/boolean-shortcuts
               resyntax/default-recommendations/class-shortcuts
               resyntax/default-recommendations/comparison-shortcuts
               resyntax/default-recommendations/conditional-shortcuts
               resyntax/default-recommendations/console-io-suggestions
               resyntax/default-recommendations/contract-shortcuts
               resyntax/default-recommendations/definition-shortcuts
               resyntax/default-recommendations/file-io-suggestions
               resyntax/default-recommendations/for-loop-shortcuts
               resyntax/default-recommendations/function-definition-shortcuts
               resyntax/default-recommendations/function-shortcuts
               resyntax/default-recommendations/hash-shortcuts
               resyntax/default-recommendations/legacy-contract-migrations
               resyntax/default-recommendations/legacy-struct-migrations
               resyntax/default-recommendations/legacy-syntax-migrations
               resyntax/default-recommendations/let-binding-suggestions
               resyntax/default-recommendations/list-shortcuts
               resyntax/default-recommendations/match-shortcuts
               resyntax/default-recommendations/miscellaneous-suggestions
               resyntax/default-recommendations/numeric-shortcuts
               resyntax/default-recommendations/require-and-provide-suggestions
               resyntax/default-recommendations/string-shortcuts
               resyntax/default-recommendations/syntax-shortcuts
               resyntax/default-recommendations/syntax-parse-shortcuts
               resyntax/default-recommendations/syntax-rules-shortcuts
               resyntax/default-recommendations/unused-binding-suggestions)
 (contract-out
  [default-recommendations refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/boolean-shortcuts
         resyntax/default-recommendations/class-shortcuts
         resyntax/default-recommendations/comparison-shortcuts
         resyntax/default-recommendations/conditional-shortcuts
         resyntax/default-recommendations/console-io-suggestions
         resyntax/default-recommendations/contract-shortcuts
         resyntax/default-recommendations/definition-shortcuts
         resyntax/default-recommendations/file-io-suggestions
         resyntax/default-recommendations/for-loop-shortcuts
         resyntax/default-recommendations/function-definition-shortcuts
         resyntax/default-recommendations/function-shortcuts
         resyntax/default-recommendations/hash-shortcuts
         resyntax/default-recommendations/legacy-contract-migrations
         resyntax/default-recommendations/legacy-struct-migrations
         resyntax/default-recommendations/legacy-syntax-migrations
         resyntax/default-recommendations/let-binding-suggestions
         resyntax/default-recommendations/list-shortcuts
         resyntax/default-recommendations/match-shortcuts
         resyntax/default-recommendations/miscellaneous-suggestions
         resyntax/default-recommendations/numeric-shortcuts
         resyntax/default-recommendations/require-and-provide-suggestions
         resyntax/default-recommendations/string-shortcuts
         resyntax/default-recommendations/syntax-parse-shortcuts
         resyntax/default-recommendations/syntax-rules-shortcuts
         resyntax/default-recommendations/syntax-shortcuts
         resyntax/default-recommendations/unused-binding-suggestions)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-suite default-recommendations
  #:suites (boolean-shortcuts
            class-shortcuts
            comparison-shortcuts
            conditional-shortcuts
            console-io-suggestions
            contract-shortcuts
            definition-shortcuts
            file-io-suggestions
            for-loop-shortcuts
            function-definition-shortcuts
            function-shortcuts
            hash-shortcuts
            legacy-contract-migrations

            ;; Excluded for lots of reasons. See the following github issues:
            ;; - jackfirth/resyntax#47
            ;; - sorawee/fmt#29
            ;; - sorawee/fmt#60
            ;; - sorawee/fmt#65
            ;; legacy-struct-migrations

            legacy-syntax-migrations
            let-binding-suggestions
            list-shortcuts
            match-shortcuts
            miscellaneous-suggestions
            numeric-shortcuts
            require-and-provide-suggestions
            string-shortcuts
            syntax-shortcuts
            syntax-parse-shortcuts
            syntax-rules-shortcuts
            unused-binding-suggestions))
