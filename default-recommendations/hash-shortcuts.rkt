#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [hash-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/set
         rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/literal-constant
         resyntax/default-recommendations/private/pure-expression
         resyntax/default-recommendations/private/syntax-equivalence
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule hash-ref-with-constant-lambda-to-hash-ref-without-lambda
  #:description "The lambda can be removed from the failure result in this `hash-ref` expression."
  #:literals (hash-ref)
  [((~and ref hash-ref) h:expr k:expr (~and lambda-expr (_:lambda-by-any-name () v:literal-constant)))
   (ref h k (ORIGINAL-GAP k lambda-expr) v)])


(define-refactoring-rule hash-ref!-with-constant-lambda-to-hash-ref!-without-lambda
  #:description "The lambda can be removed from the failure result in this `hash-ref!` expression."
  #:literals (hash-ref!)
  [((~and ref hash-ref!) h:expr k:expr
                         (~and lambda-expr (_:lambda-by-any-name () v:literal-constant)))
   (ref h k (ORIGINAL-GAP k lambda-expr) v)])


(define-syntax-class value-initializer
  #:attributes (failure-result-form)
  (pattern failure-result-form:literal-constant)
  (pattern (failure-result-form:id))
  (pattern expr:expr #:with failure-result-form #'(λ () expr)))


(define-refactoring-rule hash-ref-set!-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent `hash-ref!` expression."
  #:literals (hash-ref hash-set! define)
  [(hash-ref
    h1:id
    k1:pure-expression
    (_:lambda-by-any-name
     ()
     (define v1:id initializer:value-initializer)
     (hash-set! h2:id k2:pure-expression v2:id)
     v3:id))
   #:when (free-identifier=? #'h1 #'h2)
   #:when (syntax-free-identifier=? #'k1 #'k2)
   #:when (free-identifier=? #'v1 #'v2)
   #:when (free-identifier=? #'v1 #'v3)
   (hash-ref! h1 k1 initializer.failure-result-form)])


(define-refactoring-rule hash-ref-set!-with-constant-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent `hash-ref!` expression."
  #:literals (hash-ref hash-set! define)
  [(hash-ref
    h1:id
    k1:pure-expression
    (_:lambda-by-any-name
     ()
     (hash-set! h2:id k2:pure-expression v1:literal-constant)
     v2:literal-constant))
   #:when (free-identifier=? #'h1 #'h2)
   #:when (syntax-free-identifier=? #'k1 #'k2)
   #:when (equal? (attribute v1.value) (attribute v2.value))
   (hash-ref! h1 k1 v1)])


(define-refactoring-rule hash-set!-ref-to-hash-update!
  #:description
  "This expression can be replaced with a simpler, equivalent `hash-update!` expression."
  #:literals (hash-ref hash-set!)
  [(hash-set! h1:id k1:pure-expression
              (f:id arg-before:expr ...
                    (hash-ref h2:id k2:pure-expression (~optional failure-result))
                    arg-after:expr ...))
   #:when (free-identifier=? #'h1 #'h2)
   #:when (syntax-free-identifier=? #'k1 #'k2)
   #:when (for/and ([id (in-syntax-identifiers #'(f arg-before ... arg-after ...))])
            (not (equal? (syntax-e id) 'v)))
   (hash-update! h1 k1
                 (λ (v) (f arg-before ... v arg-after ...))
                 (~? failure-result))])


(define hash-shortcuts
  (refactoring-suite
   #:name (name hash-shortcuts)
   #:rules (list hash-ref-set!-to-hash-ref!
                 hash-ref-set!-with-constant-to-hash-ref!
                 hash-ref-with-constant-lambda-to-hash-ref-without-lambda
                 hash-ref!-with-constant-lambda-to-hash-ref!-without-lambda
                 hash-set!-ref-to-hash-update!)))
