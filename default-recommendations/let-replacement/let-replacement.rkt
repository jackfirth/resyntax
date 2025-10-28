#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [let-replacement refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/set
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/private/syntax-replacement
         syntax/id-set
         syntax/parse)


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


(define-refactoring-suite let-replacement
  #:rules (let-to-define
           define-let-to-double-define
           begin0-let-to-define-begin0))
