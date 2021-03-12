#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [hash-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule hash-ref-set!-to-hash-ref!
  #:description "This expression can be replaced with a simpler, equivalent hash-ref! expression."
  #:literals (hash-ref hash-set! define)
  [(hash-ref
    h1:id
    k1:id
    (_:lambda-by-any-name
     ()
     (define v1:id value-expr:expr)
     (hash-set! h2:id k2:id v2:id)
     v3:id))
   #:when (free-identifier=? #'h1 #'h2)
   #:when (free-identifier=? #'k1 #'k2)
   #:when (free-identifier=? #'v1 #'v2)
   #:when (free-identifier=? #'v1 #'v3)
   (hash-ref! h1 k1 (λ () value-expr))])


(define hash-shortcuts
  (refactoring-suite
   #:name (name hash-shortcuts)
   #:rules (list hash-ref-set!-to-hash-ref!)))
