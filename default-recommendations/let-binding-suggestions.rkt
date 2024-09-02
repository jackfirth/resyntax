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
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-definition-context-refactoring-rule let-to-define
  #:description
  "Internal definitions are recommended instead of `let` expressions, to reduce nesting."
  let-expr:body-with-refactorable-let-expression
  (let-expr.refactored ...))


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


(define-definition-context-refactoring-rule define-let-to-double-define
  #:description "This `let` expression can be pulled up into a `define` expression."
  #:literals (define let)
  (~seq body-before ...
        (define id:id (let ([nested-id:id nested-expr:expr]) expr:expr))
        body-after ...)
  #:when (not
          (set-member? (syntax-bound-identifiers #'(body-before ... id body-after ...)) #'nested-id))
  (body-before ...
   (define nested-id nested-expr)
   (define id expr)
   body-after ...))


(define-definition-context-refactoring-rule begin0-let-to-define-begin0
  #:description "This `let` expression can be pulled up into a `define` expression."
  #:literals (begin0 let)
  (~seq body-before ...
        (begin0
            (~and original-let (let ([nested-id:id nested-expr:expr]) result-expr:expr))
          body-after ...))
  #:when (not
          (set-member? (syntax-bound-identifiers #'(body-before ... body-after ...)) #'nested-id))
  (body-before ...
   (define nested-id nested-expr)
   (begin0 (~replacement result-expr #:original original-let) body-after ...)))


(define-refactoring-rule delete-redundant-let
  #:description "This `let` binding does nothing and can be removed."
  #:literals (let)
  (let ([left-id:id right-id:id]) body)
  #:when (bound-identifier=? #'left-id #'right-id)
  body)


(define let-binding-suggestions
  (refactoring-suite
   #:name (name let-binding-suggestions)
   #:rules
   (list begin0-let-to-define-begin0
         define-let-to-double-define
         delete-redundant-let
         let-to-define
         let-values-then-call-to-call-with-values
         named-let-to-plain-let)))
