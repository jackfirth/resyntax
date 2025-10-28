#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [let-binding-suggestions refactoring-suite?]))


(require racket/list
         racket/set
         resyntax/base
         resyntax/default-recommendations/private/syntax-equivalence
         resyntax/default-recommendations/private/syntax-identifier-sets
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule named-let-to-plain-let
  #:description
  "This named `let` loop doesn't actually perform any recursive calls, and can be replaced with an\
 unnamed `let`."
  #:literals (let)
  (let name:id header body ...)
  #:when (not (set-member? (syntax-free-identifiers #'(body ...)) #'name))
  (let header body ...))


(define-refactoring-rule let-values-then-call-to-call-with-values
  #:description
  "This `let-values` expression can be replaced with a simpler, equivalent `call-with-values`\
 expression."
  #:literals (let-values)
  (let-values ([(bound-id:id ...+) expr])
    (receiver:id arg-id:id ...+))
  #:when (syntax-free-identifier=? #'(bound-id ...) #'(arg-id ...))
  (call-with-values (λ () expr) receiver))


(define-refactoring-rule delete-redundant-let
  #:description "This `let` binding does nothing and can be removed."
  #:literals (let)
  (let ([left-id:id right-id:id]) body)
  #:when (equal? (syntax-e (attribute left-id)) (syntax-e (attribute right-id)))
  body)


(define-refactoring-suite let-binding-suggestions
  #:rules (delete-redundant-let
           let-values-then-call-to-call-with-values
           named-let-to-plain-let))
