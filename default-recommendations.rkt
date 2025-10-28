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
               resyntax/default-recommendations/dict-suggestions
               resyntax/default-recommendations/exception-suggestions
               resyntax/default-recommendations/file-io-suggestions
               resyntax/default-recommendations/function-definition-shortcuts
               resyntax/default-recommendations/function-shortcuts
               resyntax/default-recommendations/hash-shortcuts
               resyntax/default-recommendations/legacy-contract-migrations
               resyntax/default-recommendations/legacy-struct-migrations
               resyntax/default-recommendations/legacy-syntax-migrations
               resyntax/default-recommendations/let-replacement/let-binding-suggestions
               resyntax/default-recommendations/list-shortcuts
               resyntax/default-recommendations/loops/for-loop-shortcuts
               resyntax/default-recommendations/loops/list-loopification
               resyntax/default-recommendations/loops/named-let-loopification
               resyntax/default-recommendations/match-shortcuts
               resyntax/default-recommendations/miscellaneous-suggestions
               resyntax/default-recommendations/mutability-predicates
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
         resyntax/default-recommendations/dict-suggestions
         resyntax/default-recommendations/exception-suggestions
         resyntax/default-recommendations/file-io-suggestions
         resyntax/default-recommendations/function-definition-shortcuts
         resyntax/default-recommendations/function-shortcuts
         resyntax/default-recommendations/hash-shortcuts
         resyntax/default-recommendations/legacy-contract-migrations
         resyntax/default-recommendations/legacy-struct-migrations
         resyntax/default-recommendations/legacy-syntax-migrations
         resyntax/default-recommendations/let-replacement/let-binding-suggestions
         resyntax/default-recommendations/list-shortcuts
         resyntax/default-recommendations/loops/for-loop-shortcuts
         resyntax/default-recommendations/loops/list-loopification
         resyntax/default-recommendations/loops/named-let-loopification
         resyntax/default-recommendations/match-shortcuts
         resyntax/default-recommendations/miscellaneous-suggestions
         resyntax/default-recommendations/mutability-predicates
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
            dict-suggestions
            exception-suggestions
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
            list-loopification
            list-shortcuts
            match-shortcuts
            miscellaneous-suggestions
            mutability-predicates
            named-let-loopification
            numeric-shortcuts
            require-and-provide-suggestions
            string-shortcuts
            syntax-shortcuts
            syntax-parse-shortcuts
            syntax-rules-shortcuts

            ;; Excluded because of https://github.com/jackfirth/resyntax/issues/410
            ;; unused-binding-suggestions

            ))
