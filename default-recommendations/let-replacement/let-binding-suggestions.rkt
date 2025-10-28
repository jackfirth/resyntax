#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [let-binding-suggestions refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/set
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/default-recommendations/private/syntax-equivalence
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-replacement
         syntax/id-set
         syntax/parse
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
                  define/private))


;@----------------------------------------------------------------------------------------------------


(define-definition-context-refactoring-rule let-to-define
  #:description
  "Internal definitions are recommended instead of `let` expressions, to reduce nesting."
  (~seq leading-body ... let-expression:refactorable-let-expression)
  #:with (replacement ...)
  (if (empty? (attribute leading-body))
      (attribute let-expression.refactored)
      #'(~focus-replacement-on (let-expression.refactored ...)))
  (leading-body ... replacement ...))


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
        (~and original-definition (define id:id (let ([nested-id:id nested-expr:expr]) expr:expr)))
        body-after ...)
  #:when (identifier-binding-unchanged-in-context? (attribute id) (attribute nested-expr))
  #:when (for/and ([body-free-id
                    (in-free-id-set
                     (syntax-free-identifiers #'(body-before ... nested-expr body-after ...)))])
           (identifier-binding-unchanged-in-context? body-free-id (attribute nested-id)))
  (body-before ...
   (~@ . (~focus-replacement-on
          (~splicing-replacement ((define nested-id nested-expr) (define id expr))
                                 #:original original-definition)))
   body-after ...))


(define-definition-context-refactoring-rule begin0-let-to-define-begin0
  #:description
  "The `let` expression in this `begin0` form can be extracted into the surrounding definition\
 context."
  #:literals (begin0 let)
  (~seq body-before ...
        (begin0
            (~and original-let (let ([nested-id:id nested-expr:expr]) let-body ... result-expr:expr))
          body-after ...))
  #:when (not
          (set-member? (syntax-bound-identifiers #'(body-before ... body-after ...)) #'nested-id))
  (body-before ...
   (define nested-id nested-expr)
   let-body ...
   (begin0 (~replacement result-expr #:original original-let) body-after ...)))


(define-refactoring-rule delete-redundant-let
  #:description "This `let` binding does nothing and can be removed."
  #:literals (let)
  (let ([left-id:id right-id:id]) body)
  #:when (equal? (syntax-e (attribute left-id)) (syntax-e (attribute right-id)))
  body)


(define-refactoring-suite let-binding-suggestions
  #:rules (let-to-define
           begin0-let-to-define-begin0
           define-let-to-double-define
           delete-redundant-let
           let-values-then-call-to-call-with-values
           named-let-to-plain-let))
