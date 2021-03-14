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
  #:description "Internal definitions are recommended instead of let expressions, to reduce nesting."
  [(header:header-form-allowing-internal-definitions let-expr:body-with-refactorable-let-expression)
   (header.formatted ... let-expr.refactored ...)])


(define-splicing-syntax-class header-form-allowing-internal-definitions
  #:attributes ([formatted 1])
  #:literals (let
                 let*
               let-values
               let*-values
               when
               unless
               with-handlers
               parameterize
               for
               for/list
               for/hash
               for/hasheq
               for/hasheqv
               for/and
               for/or
               for/sum
               for/product
               for/first
               for/last
               for*
               for*/list
               for*/hash
               for*/hasheq
               for*/hasheqv
               for*/and
               for*/or
               for*/sum
               for*/product
               for*/first
               for*/last)

  (pattern (~seq lambda:lambda-by-any-name ~! formals:formals)
    #:with (formatted ...) #'(lambda formals))

  (pattern (~seq define:define-by-any-name ~! header:function-header)
    #:with (formatted ...) #'(define header))

  (pattern (~seq let ~! (~optional name:id) header)
    #:with (formatted ...) #'(let (~? name) header))

  (pattern (~seq let* ~! header)
    #:with (formatted ...) #'(let* header))

  (pattern (~seq let-values ~! header)
    #:with (formatted ...) #'(let-values header))

  (pattern (~seq (~and id let*-values) ~! header)
    #:with (formatted ...) #'((ORIGINAL-SPLICE id header)))

  (pattern (~seq when ~! condition)
    #:with (formatted ...) #'(when condition))

  (pattern (~seq unless ~! condition)
    #:with (formatted ...) #'(unless condition))

  (pattern (~seq with-handlers ~! handlers)
    #:with (formatted ...) #'(with-handlers handlers))

  (pattern (~seq parameterize ~! handlers)
    #:with (formatted ...) #'(parameterize handlers))

  (pattern
      (~seq
       (~and for-id
             (~or for
                  for/list
                  for/hash
                  for/hasheq
                  for/hasheqv
                  for/and
                  for/or
                  for/sum
                  for/product
                  for/first
                  for/last
                  for*
                  for*/list
                  for*/hash
                  for*/hasheq
                  for*/hasheqv
                  for*/and
                  for*/or
                  for*/sum
                  for*/product
                  for*/first
                  for*/last))
       ~!
       clauses)
    #:with (formatted ...) #'((ORIGINAL-SPLICE for-id clauses))))


;; There's a lot of variants of define that support the same grammar but have different meanings. We
;; can recognize and refactor all of them with this syntax class.
(define-syntax-class define-by-any-name
  #:literals (define
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
  (pattern
      (~or define
           define/augment
           define/augment-final
           define/augride
           define/overment
           define/override
           define/override-final
           define/public
           define/public-final
           define/pubment
           define/private)))


(define-refactoring-rule named-let-to-plain-let
  #:description
  "This named let loop doesn't actually perform any recursive calls, and can be replaced with an\
 unnamed let."
  #:literals (let)
  [(let name:id header body ...)
   #:when (not (set-member? (syntax-free-identifiers #'(body ...)) #'name))
   (let (ORIGINAL-SPLICE header body ...))])


(define-refactoring-rule let-values-then-call-to-call-with-values
  #:description
  "This let-values expression can be replaced with a simpler, equivalent call-with-values expression."
  #:literals (let-values)
  [(let-values ([(bound-id:id ...+) expr])
     (receiver:id arg-id:id ...+))
   #:when (syntax-free-identifier=? #'(bound-id ...) #'(arg-id ...))
   (call-with-values (λ () expr) receiver)])


(define let-binding-suggestions
  (refactoring-suite
   #:name (name let-binding-suggestions)
   #:rules (list let-to-define let-values-then-call-to-call-with-values named-let-to-plain-let)))
