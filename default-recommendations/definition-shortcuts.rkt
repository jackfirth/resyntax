#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/private/static-name
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-definition-context-refactoring-rule define-values-values-to-define
  #:description "This use of `define-values` is unnecessary."
  #:literals (define-values values)
  (~seq body-before ...
        (define-values (id:id ...) (values expr:expr ...))
        body-after ...)
  (body-before ... (define id expr) ... body-after ...))


(define-definition-context-refactoring-rule inline-unnecessary-define
  #:description "This variable is returned immediately and can be inlined."
  #:literals (define)
  (~seq body-before ... (~and definition (define id1:id expr)) id2:id)
  #:when (free-identifier=? #'id1 #'id2)
  #:with replacement #'(~replacement expr #:original-splice (definition id2))
  #:with focused (if (empty? (attribute body-before))
                     #'replacement
                     #'(~focus-replacement-on replacement))
  (body-before ... focused))


(define-refactoring-suite definition-shortcuts
  #:rules (define-values-values-to-define
            inline-unnecessary-define))
