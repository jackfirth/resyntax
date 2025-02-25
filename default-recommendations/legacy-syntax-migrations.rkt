#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [legacy-syntax-migrations refactoring-suite?]))


(require (for-syntax racket/base)
         (for-template racket/base
                       racket/match
                       racket/provide-syntax
                       racket/require-syntax
                       syntax/parse)
         rebellion/private/static-name
         resyntax/base
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule datum->syntax-migration
  #:description "The fifth argument to `datum->syntax` is ignored."
  #:literals (datum->syntax)
  ((~and id datum->syntax) ctxt v srcloc prop ignored)
  (id ctxt v srcloc prop))


(define-refactoring-rule syntax-recertify-migration
  #:description "The `syntax-recertify` function is a legacy function that does nothing."
  #:literals (syntax-recertify)
  (syntax-recertify stx _ _ _)
  stx)


(define-refactoring-rule syntax-disarm-migration
  #:description "The `syntax-disarm` function is a legacy function that does nothing."
  #:literals (syntax-disarm)
  (syntax-disarm stx _)
  stx)


(define-refactoring-rule syntax-rearm-migration
  #:description "The `syntax-rearm` function is a legacy function that does nothing."
  #:literals (syntax-rearm)
  (syntax-rearm stx _ ...)
  stx)


(define-refactoring-rule syntax-protect-migration
  #:description "The `syntax-protect` function is a legacy function that does nothing."
  #:literals (syntax-protect)
  (syntax-protect stx)
  stx)


(define-refactoring-rule for-clause-syntax-protect-migration
  #:description "The `for-clause-syntax-protect` function is a legacy function that does nothing."
  #:literals (for-clause-syntax-protect)
  (for-clause-syntax-protect stx)
  stx)


(define-refactoring-rule syntax-local-match-introduce-migration
  #:description
  "The `syntax-local-match-introduce` function is a legacy function that's equivalent to\
 `syntax-local-introduce`."
  #:literals (syntax-local-match-introduce)
  (id:syntax-local-match-introduce stx)
  ((~replacement syntax-local-introduce #:original id) stx))


(define-refactoring-rule syntax-local-provide-introduce-migration
  #:description
  "The `syntax-local-provide-introduce` function is a legacy function that's equivalent to\
 `syntax-local-introduce`."
  #:literals (syntax-local-provide-introduce)
  (id:syntax-local-provide-introduce stx)
  ((~replacement syntax-local-introduce #:original id) stx))


(define-refactoring-rule syntax-local-require-introduce-migration
  #:description
  "The `syntax-local-require-introduce` function is a legacy function that's equivalent to\
 `syntax-local-introduce`."
  #:literals (syntax-local-require-introduce)
  (id:syntax-local-require-introduce stx)
  ((~replacement syntax-local-introduce #:original id) stx))


(define-refactoring-rule syntax-local-syntax-parse-pattern-introduce-migration
  #:description
  "The `syntax-local-syntax-parse-pattern-introduce` function is a legacy function that's equivalent\
 to `syntax-local-introduce`."
  #:literals (syntax-local-syntax-parse-pattern-introduce)
  (id:syntax-local-syntax-parse-pattern-introduce stx)
  ((~replacement syntax-local-introduce #:original id) stx))


(define-refactoring-suite legacy-syntax-migrations
  #:rules (datum->syntax-migration
           for-clause-syntax-protect-migration
           syntax-disarm-migration
           syntax-local-match-introduce-migration
           syntax-local-provide-introduce-migration
           syntax-local-require-introduce-migration
           syntax-local-syntax-parse-pattern-introduce-migration
           syntax-protect-migration
           syntax-rearm-migration
           syntax-recertify-migration))
