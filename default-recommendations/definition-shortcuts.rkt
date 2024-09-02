#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/definition-context
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-definition-context-refactoring-rule define-values-values-to-define
  #:description "This use of `define-values` is unnecessary."
  #:literals (define-values values)
  [(~seq body-before ...
         (define-values (id:id ...) (values expr:expr ...))
         body-after ...)
   (body-before ... (define id expr) ... body-after ...)])


(define-definition-context-refactoring-rule inline-unnecessary-define
  #:description "This variable is returned immediately and can be inlined."
  #:literals (define)
  [(~seq body-before ... (define id1:id expr) id2)
   #:when (free-identifier=? #'id1 #'id2)
   (body-before ... expr)])


(define definition-shortcuts
  (refactoring-suite
   #:name (name definition-shortcuts)
   #:rules
   (list define-values-values-to-define
         inline-unnecessary-define)))
