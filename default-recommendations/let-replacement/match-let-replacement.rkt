#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [match-let-replacement refactoring-suite?]))


(require racket/list
         racket/match
         racket/set
         resyntax/base
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/strip-context)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class single-binding-match-let
  #:literals (match-let match-let* match-letrec)
  #:attributes (match-pattern subject [body 1] [as-definition-context-body 1])

  (pattern ((~or match-let match-let* match-letrec) ([match-pattern subject]) body ...)
    #:with definition #'(match-define match-pattern subject)
    #:with (as-definition-context-body ...)
    #`(~splicing-replacement (definition body ...) #:original #,this-syntax)))


(define-definition-context-refactoring-rule match-let-to-match-define
  #:description
  "Internal definitions are recommended instead of `match-let` expressions with a single binding, to\
 reduce nesting."
  #:literals (match-let match-let* match-letrec)
  (~seq body-before ... match-let-expression:single-binding-match-let)

  #:do
  [(define pattern-ids
     (syntax-bound-identifiers (attribute match-let-expression.match-pattern)))
   (define pattern-ids-in-surrounding-context
     (syntax-bound-identifiers
      (replace-context (attribute match-let-expression) (attribute match-let-expression.match-pattern))))
   (define body-ids (syntax-bound-identifiers #'(body-before ... match-let-expression.subject)))
   (define subject-ids-in-body-context
     (syntax-bound-identifiers
      (replace-context
       (first (attribute match-let-expression.body)) (attribute match-let-expression.subject))))]
  #:when (set-empty? (set-intersect pattern-ids-in-surrounding-context body-ids))
  #:when (set-empty? (set-intersect pattern-ids subject-ids-in-body-context))
  #:with (new-body ...)
  (if (empty? (attribute body-before))
      (attribute match-let-expression.as-definition-context-body)
      #'(~focus-replacement-on
         (match-let-expression.as-definition-context-body ...)))

  (body-before ... new-body ...))


(define-refactoring-suite match-let-replacement
  #:rules (match-let-to-match-define))
