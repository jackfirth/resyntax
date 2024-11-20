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
        (~and definition (define-values (id:id ...) (values expr:expr ...)))
        body-after ...)
  #:with (replacement ...)
  #'(~focus-replacement-on (~splicing-replacement ((define id expr) ...) #:original definition))
  (body-before ... replacement ... body-after ...))


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


(define-definition-context-refactoring-rule define-begin-extraction
  #:description
  "The `begin` in this definition can be extracted into the surrounding definition context."
  #:literals (define begin)
  (~seq body-before ...
        (~and definition (define id (begin pre-body ... expr)))
        body-after ...)
  #:with (replacement ...)
  #'(~focus-replacement-on
     (~splicing-replacement (pre-body ... (define id expr)) #:original definition))
  (body-before ... replacement ... body-after ...))


(define-definition-context-refactoring-rule define-begin0-extraction
  #:description
  "The `begin0` in this definition can be extracted into the surrounding definition context."
  #:literals (define begin0)
  (~seq body-before ...
        (~and definition (define id (begin0 expr post-body ...)))
        body-after ...)
  #:with (replacement ...)
  #'(~focus-replacement-on
     (~splicing-replacement ((define id expr) post-body ...) #:original definition))
  (body-before ... replacement ... body-after ...))


(define-refactoring-suite definition-shortcuts
  #:rules (define-begin-extraction
            define-begin0-extraction
            define-values-values-to-define
            inline-unnecessary-define))
