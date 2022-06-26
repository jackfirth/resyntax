#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [let-binding-suggestions refactoring-suite?]))


(require (for-syntax racket/base)
         (only-in racket/class
                  define/augment
                  define/augment-final
                  define/augride
                  define/overment
                  define/override
                  define/override-final
                  define/public
                  define/public-final
                  define/pubment
                  define/private)
         racket/set
         rebellion/private/static-name
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/let-binding
         resyntax/default-recommendations/private/syntax-equivalence
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse
         syntax/parse/lib/function-header)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule let-to-define
  #:description "Internal definitions are recommended instead of `let` expressions, to reduce nesting."
  [(header:header-form-allowing-internal-definitions let-expr:body-with-refactorable-let-expression)
   (header.formatted ... let-expr.refactored ...)])


(define-refactoring-rule named-let-to-plain-let
  #:description
  "This named `let` loop doesn't actually perform any recursive calls, and can be replaced with an\
 unnamed `let`."
  #:literals (let)
  [(let name:id header body ...)
   #:when (not (set-member? (syntax-free-identifiers #'(body ...)) #'name))
   (let (ORIGINAL-SPLICE header body ...))])


(define-refactoring-rule let-values-then-call-to-call-with-values
  #:description
  "This `let-values` expression can be replaced with a simpler, equivalent `call-with-values` expression."
  #:literals (let-values)
  [(let-values ([(bound-id:id ...+) expr])
     (receiver:id arg-id:id ...+))
   #:when (syntax-free-identifier=? #'(bound-id ...) #'(arg-id ...))
   (call-with-values (λ () expr) receiver)])


(define let-binding-suggestions
  (refactoring-suite
   #:name (name let-binding-suggestions)
   #:rules (list let-to-define let-values-then-call-to-call-with-values named-let-to-plain-let)))
