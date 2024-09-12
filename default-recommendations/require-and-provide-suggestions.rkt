#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [require-and-provide-suggestions refactoring-suite?]))


(require racket/list
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class export-spec
  #:attributes (id)
  (pattern id:id))


(define-refactoring-rule provide-deduplication
  #:description "Providing the same identifier multiple times is unnecessary."
  #:literals (provide)
  (provide spec:export-spec ...)
  #:when (check-duplicate-identifier (attribute spec.id))

  #:with (deduped-spec ...)
  (remove-duplicates (attribute spec) bound-identifier=?
                     #:key (λ (spec-stx) (syntax-parse spec-stx [:export-spec #'id])))

  (provide deduped-spec ...))


(define require-and-provide-suggestions
  (refactoring-suite
   #:name (name require-and-provide-suggestions)
   #:rules (list provide-deduplication)))
