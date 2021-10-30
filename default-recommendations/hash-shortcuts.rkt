#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [hash-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/literal-constant
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class value-initializer
  #:attributes (failure-result-form)
  (pattern failure-result-form:literal-constant)
  (pattern (failure-result-form:id))
  (pattern expr:expr #:with failure-result-form #'(λ () expr)))


(define-refactoring-rule hash-ref-set!-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent hash-ref! expression."
  #:literals (hash-ref hash-set! define)
  [(hash-ref
    h1:id
    k1:id
    (_:lambda-by-any-name
     ()
     (define v1:id initializer:value-initializer)
     (hash-set! h2:id k2:id v2:id)
     v3:id))
   #:when (free-identifier=? #'h1 #'h2)
   #:when (free-identifier=? #'k1 #'k2)
   #:when (free-identifier=? #'v1 #'v2)
   #:when (free-identifier=? #'v1 #'v3)
   (hash-ref! h1 k1 initializer.failure-result-form)])


(define-refactoring-rule hash-ref-set!-with-constant-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent hash-ref! expression."
  #:literals (hash-ref hash-set! define)
  [(hash-ref
    h1:id
    k1:id
    (_:lambda-by-any-name
     ()
     (hash-set! h2:id k2:id v1:literal-constant)
     v2:literal-constant))
   #:when (free-identifier=? #'h1 #'h2)
   #:when (free-identifier=? #'k1 #'k2)
   #:when (equal? (attribute v1.value) (attribute v2.value))
   (hash-ref! h1 k1 v1)])


(define hash-shortcuts
  (refactoring-suite
   #:name (name hash-shortcuts)
   #:rules (list hash-ref-set!-to-hash-ref!
                 hash-ref-set!-with-constant-to-hash-ref!)))
