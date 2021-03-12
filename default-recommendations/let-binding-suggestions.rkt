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
         rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/let-binding
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


(define let-binding-suggestions
  (refactoring-suite
   #:name (name let-binding-suggestions)
   #:rules (list let-to-define)))
