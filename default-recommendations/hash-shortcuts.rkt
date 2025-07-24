#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [hash-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/set
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/literal-constant
         resyntax/default-recommendations/private/pure-expression
         resyntax/default-recommendations/private/syntax-equivalence
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule hash-ref-with-constant-lambda-to-hash-ref-without-lambda
  #:description "The lambda can be removed from the failure result in this `hash-ref` expression."
  #:literals (hash-ref)
  ((~and ref hash-ref) h:expr k:expr (~and lambda-expr (_:lambda-by-any-name () v:literal-constant)))
  (ref h k (~replacement v #:original lambda-expr)))


(define-refactoring-rule hash-ref!-with-constant-lambda-to-hash-ref!-without-lambda
  #:description "The lambda can be removed from the failure result in this `hash-ref!` expression."
  #:literals (hash-ref!)
  ((~and ref hash-ref!) h:expr k:expr
                        (~and lambda-expr (_:lambda-by-any-name () v:literal-constant)))
  (ref h k (~replacement v #:original lambda-expr)))


(define-syntax-class value-initializer
  #:attributes (failure-result-form)
  (pattern failure-result-form:literal-constant)
  (pattern (failure-result-form:id))
  (pattern expr:expr #:with failure-result-form #'(λ () expr)))


(define-refactoring-rule hash-ref-set!-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent `hash-ref!` expression."
  #:literals (hash-ref hash-set! define)
  (hash-ref
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
  (hash-ref! h1 k1 initializer.failure-result-form))


(define-refactoring-rule hash-ref-set!-with-constant-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent `hash-ref!` expression."
  #:literals (hash-ref hash-set! define)
  (hash-ref
   h1:id
   k1:pure-expression
   (_:lambda-by-any-name
    ()
    (hash-set! h2:id k2:pure-expression v1:literal-constant)
    v2:literal-constant))
  #:when (free-identifier=? #'h1 #'h2)
  #:when (syntax-free-identifier=? #'k1 #'k2)
  #:when (equal? (attribute v1.value) (attribute v2.value))
  (hash-ref! h1 k1 v1))


(define-refactoring-rule or-hash-ref-set!-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent `hash-ref!` expression."
  #:literals (or hash-ref hash-set! let)
  (or (hash-ref h1:id k1:pure-expression #false)
      (let ([v1:id initializer:value-initializer])
        (hash-set! h2:id k2:pure-expression v2:id)
        v3:id))
  #:when (free-identifier=? #'h1 #'h2)
  #:when (syntax-free-identifier=? #'k1 #'k2)
  #:when (free-identifier=? #'v1 #'v2)
  #:when (free-identifier=? #'v1 #'v3)
  (hash-ref! h1 k1 initializer.failure-result-form))


(define-refactoring-rule hash-set!-ref-to-hash-update!
  #:description
  "This expression can be replaced with a simpler, equivalent `hash-update!` expression."
  #:literals (hash-ref hash-set!)
  (hash-set! h1:id k1:pure-expression
             (f:id arg-before:expr ...
                   (hash-ref h2:id k2:pure-expression (~optional failure-result))
                   arg-after:expr ...))
  #:when (free-identifier=? #'h1 #'h2)
  #:when (syntax-free-identifier=? #'k1 #'k2)
  #:when (for/and ([id (in-syntax-identifiers #'(f arg-before ... arg-after ...))])
           (not (equal? (syntax-e id) 'v)))
  #:with updater
  (if (and (empty? (attribute arg-before)) (empty? (attribute arg-after)))
      #'f
      #'(λ (v) (f arg-before ... v arg-after ...)))
  (hash-update! h1 k1 updater (~? failure-result)))


(define-refactoring-rule let-hash-ref!-set!-to-hash-update!
  #:description
  "This expression can be replaced with a simpler, equivalent `hash-update!` expression."
  #:literals (let hash-ref! hash-set!)
  (let ([v1:id (hash-ref! h1:id k1:pure-expression failure-result)])
    (hash-set! h2:id k2:pure-expression 
               (f:id arg-before:expr ... v2:id arg-after:expr ...)))
  #:when (free-identifier=? #'h1 #'h2)
  #:when (syntax-free-identifier=? #'k1 #'k2)
  #:when (free-identifier=? #'v1 #'v2)
  #:with updater
  (if (and (empty? (attribute arg-before)) (empty? (attribute arg-after)))
      #'f
      #`(λ (#,#'v1) (f arg-before ... #,#'v1 arg-after ...)))
  #:with final-failure-result
  (syntax-parse #'failure-result
    [(_:lambda-by-any-name () v:literal-constant) #'v]
    [other #'other])
  (hash-update! h1 k1 updater final-failure-result))


(define-refactoring-rule hash-map-to-hash-keys
  #:description "This `hash-map` expression is equivalent to the `hash-keys` function."
  #:literals (hash-map)
  (hash-map h (_:lambda-by-any-name (k1:id v) k2:id))
  #:when (free-identifier=? #'k1 #'k2)
  (hash-keys h))


(define-refactoring-rule hash-map-to-hash-values
  #:description "This `hash-map` expression is equivalent to the `hash-values` function."
  #:literals (hash-map)
  (hash-map h (_:lambda-by-any-name (k v1:id) v2:id))
  #:when (free-identifier=? #'v1 #'v2)
  (hash-values h))


(define-refactoring-suite hash-shortcuts
  #:rules (hash-map-to-hash-keys
           hash-map-to-hash-values
           hash-ref-set!-to-hash-ref!
           hash-ref-set!-with-constant-to-hash-ref!
           let-hash-ref!-set!-to-hash-update!
           hash-ref-with-constant-lambda-to-hash-ref-without-lambda
           ;; Temporarily disabled: hash-ref!-with-constant-lambda-to-hash-ref!-without-lambda
           hash-set!-ref-to-hash-update!
           or-hash-ref-set!-to-hash-ref!))
