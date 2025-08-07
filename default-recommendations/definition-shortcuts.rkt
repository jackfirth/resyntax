#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/match
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
  #:when (equal? (syntax-property #'id1 'usage-count) 1)
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
        (~and definition (define id:id (begin pre-body ... expr)))
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
        (~and definition (define id:id (begin0 expr post-body ...)))
        body-after ...)
  #:with (replacement ...)
  #'(~focus-replacement-on
     (~splicing-replacement ((define id expr) post-body ...) #:original definition))
  (body-before ... replacement ... body-after ...))


(define-definition-context-refactoring-rule begin0-begin-extraction
  #:description
  "The `begin` form inside this `begin0` form can be extracted into the surrounding definition\
 context."
  #:literals (begin0 begin)
  (~seq body-before ...
        (~and outer-form (begin0 (begin pre-body ... expr) post-body ...)))
  #:with (replacement ...)
  #'(~focus-replacement-on
     (~splicing-replacement (pre-body ... (begin0 expr post-body ...)) #:original outer-form))
  (body-before ... replacement ...))


(define-definition-context-refactoring-rule inline-unnecessary-begin
  #:description "This `begin` form can be flattened into the surrounding definition context."
  #:literals (begin)
  (~seq body-before ... (~and original (begin inner-body ...)) body-after ...)

  #:with (replacement ...)
  (if (and (empty? (attribute body-before)) (empty? (attribute body-after)))

      ; If the begin form being inlined is the *only* form in the surrounding definition context,
      ; then it's possible the user has formatted it in such a way that replacing it with multiple
      ; forms could require moving those forms onto surrounding lines. In that case, we explicitly
      ; don't want to focus the replacement on just the begin forms, because we *should* try to
      ; reformat the entire surrounding context. For example, refactoring this code:
      ;
      ; (map (λ (x) (begin (f x) (g x))) xs)
      ;
      ; Should produce this code:
      ;
      ; (map (λ (x)
      ;        (f x)
      ;        (g x))
      ;      xs)
      ;
      ; And *not* this code:
      ;
      ; (map (λ (x) (f x)
      ;             (g x))
      ;      xs)
      ;
      ; To do that, we have to ensure that the whole lambda form (the entire surrounding definition
      ; context) is formatted because we're changing that context from a context with one body form
      ; to a context with potentially multiple body forms.
      #'(inner-body ...)

      ; In the case that we already know the surrounding context contains other body forms, we know
      ; we won't need to reformat the whole context. We focus the replacement on just the inline forms
      ; so that we can ignore things like comments surrounding the inlined (begin ...) form.
      #'(~focus-replacement-on (~splicing-replacement (inner-body ...) #:original original)))

  (body-before ... replacement ... body-after ...))


(define-refactoring-suite definition-shortcuts
  #:rules (begin0-begin-extraction
            define-begin-extraction
            define-begin0-extraction
            define-values-values-to-define
            inline-unnecessary-begin
            inline-unnecessary-define))
