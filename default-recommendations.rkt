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
               resyntax/default-recommendations/legacy/define-simple-macro-migration
               resyntax/default-recommendations/legacy/legacy-contract-migrations
               resyntax/default-recommendations/legacy/legacy-struct-migrations
               resyntax/default-recommendations/legacy/legacy-syntax-migrations
               resyntax/default-recommendations/legacy/make-temporary-directory-migration
               resyntax/default-recommendations/legacy/provide-contract-migration
               resyntax/default-recommendations/let-binding-suggestions
               resyntax/default-recommendations/let-replacement/argument-let-replacement
               resyntax/default-recommendations/let-replacement/cond-let-replacement
               resyntax/default-recommendations/let-replacement/let-replacement
               resyntax/default-recommendations/let-replacement/match-let-replacement
               resyntax/default-recommendations/list-shortcuts
               resyntax/default-recommendations/loops/for-loop-shortcuts
               resyntax/default-recommendations/loops/fuse-map-with-for
               resyntax/default-recommendations/loops/list-loopification
               resyntax/default-recommendations/loops/named-let-loopification
               resyntax/default-recommendations/match-shortcuts
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


(require resyntax/base
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
         resyntax/default-recommendations/legacy/define-simple-macro-migration
         resyntax/default-recommendations/legacy/legacy-contract-migrations
         resyntax/default-recommendations/legacy/legacy-struct-migrations
         resyntax/default-recommendations/legacy/legacy-syntax-migrations
         resyntax/default-recommendations/legacy/make-temporary-directory-migration
         resyntax/default-recommendations/legacy/provide-contract-migration
         resyntax/default-recommendations/let-binding-suggestions
         resyntax/default-recommendations/let-replacement/argument-let-replacement
         resyntax/default-recommendations/let-replacement/cond-let-replacement
         resyntax/default-recommendations/let-replacement/let-replacement
         resyntax/default-recommendations/let-replacement/match-let-replacement
         resyntax/default-recommendations/list-shortcuts
         resyntax/default-recommendations/loops/for-loop-shortcuts
         resyntax/default-recommendations/loops/fuse-map-with-for
         resyntax/default-recommendations/loops/list-loopification
         resyntax/default-recommendations/loops/named-let-loopification
         resyntax/default-recommendations/match-shortcuts
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
  #:suites (argument-let-replacement
            boolean-shortcuts
            class-shortcuts
            comparison-shortcuts
            conditional-shortcuts
            cond-let-replacement
            console-io-suggestions
            contract-shortcuts
            define-simple-macro-migration
            definition-shortcuts
            dict-suggestions
            exception-suggestions
            file-io-suggestions
            for-loop-shortcuts
            fuse-map-with-for
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
            let-replacement
            list-loopification
            list-shortcuts
            make-temporary-directory-migration
            match-let-replacement
            match-shortcuts
            mutability-predicates
            named-let-loopification
            numeric-shortcuts
            provide-contract-migration
            require-and-provide-suggestions
            string-shortcuts
            syntax-shortcuts
            syntax-parse-shortcuts
            syntax-rules-shortcuts

            ;; Excluded because of https://github.com/jackfirth/resyntax/issues/410
            ;; unused-binding-suggestions

            ))
