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


;; This syntax class exists solely so that the let-to-define rule can match two cases that are shaped
;; very differently.
(define-syntax-class expresion-matching-let-to-define
  #:attributes (refactored)

  (pattern (header:header-form-allowing-internal-definitions
            let-expr:body-with-refactorable-let-expression)
    #:with refactored
    #'(header.original ... let-expr.refactored ...))

  (pattern (branching-header:branching-form-allowing-internal-definitions-within-clauses
            clause-before ...
            (~and original-clause [clause-header let-expr:body-with-refactorable-let-expression])
            clause-after ...)
    #:with refactored
    #'(branching-header.original ...
       clause-before ...
       (~replacement [clause-header let-expr.refactored ...] #:original original-clause)
       clause-after ...)))


(define-refactoring-rule let-to-define
  #:description
  "Internal definitions are recommended instead of `let` expressions, to reduce nesting."
  [expression:expresion-matching-let-to-define expression.refactored])


(define-refactoring-rule named-let-to-plain-let
  #:description
  "This named `let` loop doesn't actually perform any recursive calls, and can be replaced with an\
 unnamed `let`."
  #:literals (let)
  [(let name:id header body ...)
   #:when (not (set-member? (syntax-free-identifiers #'(body ...)) #'name))
   (let header body ...)])


(define-refactoring-rule let-values-then-call-to-call-with-values
  #:description
  "This `let-values` expression can be replaced with a simpler, equivalent `call-with-values`\
 expression."
  #:literals (let-values)
  [(let-values ([(bound-id:id ...+) expr])
     (receiver:id arg-id:id ...+))
   #:when (syntax-free-identifier=? #'(bound-id ...) #'(arg-id ...))
   (call-with-values (λ () expr) receiver)])


(define-splicing-syntax-class define-with-nested-let-and-body
  #:attributes ([refactored 1])
  #:literals (define let)
  (pattern (~seq body-before ...
                 (define id:id (let ([nested-id:id nested-expr:expr]) expr:expr))
                 body-after ...)
    #:when (not (set-member? (syntax-bound-identifiers #'(body-before ... id body-after ...))
                             #'nested-id))
    #:with (refactored ...)
    #'(body-before ...
       (define nested-id nested-expr)
       (define id expr)
       body-after ...)))


;; This syntax class exists solely so that the define-let-to-double-define rule can match two cases
;; that are shaped very differently.
(define-syntax-class expression-matching-define-let-to-double-define
  #:attributes (refactored)

  (pattern (header:header-form-allowing-internal-definitions
            define-with-let:define-with-nested-let-and-body)
    
    #:with refactored
    #'(header.original ... define-with-let.refactored ...))

  (pattern (branching-header:branching-form-allowing-internal-definitions-within-clauses
            clause-before ...
            (~and original-clause [clause-header define-with-let:define-with-nested-let-and-body])
            clause-after ...)
    #:with refactored
    #'(branching-header.original ...
       clause-before ...
       (~replacement [clause-header define-with-let.refactored ...] #:original original-clause)
       clause-after ...)))


(define-refactoring-rule define-let-to-double-define
  #:description "This `let` expression can be pulled up into a `define` expression."
  [expression:expression-matching-define-let-to-double-define expression.refactored])


(define-refactoring-rule delete-redundant-let
  #:description "This `let` binding does nothing and can be removed."
  #:literals (let)
  [(let ([left-id:id right-id:id]) body)
   #:when (bound-identifier=? #'left-id #'right-id)
   body])


(define let-binding-suggestions
  (refactoring-suite
   #:name (name let-binding-suggestions)
   #:rules
   (list define-let-to-double-define
         delete-redundant-let
         let-to-define
         let-values-then-call-to-call-with-values
         named-let-to-plain-let)))
