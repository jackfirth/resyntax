#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [function-definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/private/guarded-block
         rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define/guard (free-identifiers=? ids other-ids)
  (define id-list (syntax->list ids))
  (define other-id-list (syntax->list other-ids))
  (guard (equal? (length id-list) (length other-id-list)) else
    #false)
  (for/and ([id (in-list id-list)] [other-id (in-list other-id-list)])
    (free-identifier=? id other-id)))


(define-syntax-class lambda-header
  #:attributes (converted)

  (pattern converted:id)

  (pattern ()
    #:with converted #'())

  (pattern (formal ...+)
    #:with converted #'((ORIGINAL-SPLICE formal ...)))

  (pattern (formal ... . rest-arg:id)
    #:with converted #'((ORIGINAL-SPLICE formal ... rest-arg))))


(define-syntax-class possibly-nested-lambdas
  #:attributes ([converted-formals 1] [formatted-body 1])

  (pattern (_:lambda-by-any-name first-formals:lambda-header nested:possibly-nested-lambdas)
    #:with (converted-formals ...) #'(first-formals.converted nested.converted-formals ...)
    #:with (formatted-body ...) #'(nested.formatted-body ...))

  (pattern (_:lambda-by-any-name first-formals:lambda-header initial-body body ...)
    #:with (converted-formals ...) #'(first-formals.converted)
    #:with (formatted-body ...)
    #'((ORIGINAL-GAP first-formals initial-body) (ORIGINAL-SPLICE initial-body body ...))))


(define/guard (build-function-header original-header converted-lambda-formal-lists)
  (guard (empty? converted-lambda-formal-lists) then
    original-header)
  (with-syntax ([formals (first converted-lambda-formal-lists)])
    (build-function-header #`(#,original-header . formals) (rest converted-lambda-formal-lists))))


(define-refactoring-rule define-lambda-to-define
  #:description
  "The define form supports a shorthand for defining functions (including function-returning\
 functions)."
  #:literals (define)
  [(define header lambda-form:possibly-nested-lambdas)
   #:with new-header (build-function-header #'header (attribute lambda-form.converted-formals))
   (define new-header lambda-form.formatted-body ...)])


(define-refactoring-rule define-case-lambda-to-define
  #:description "This use of case-lambda is equivalent to using define with optional arguments."
  #:literals (define case-lambda)
  [(define id:id
     (case-lambda
       [(case1-arg:id ...)
        (usage:id usage1:id ... default:expr)]
       [(case2-arg:id ... bonus-arg:id)
        body ...]))
   #:when (free-identifier=? #'id #'usage)
   #:when (free-identifiers=? #'(case1-arg ...) #'(case2-arg ...))
   #:when (free-identifiers=? #'(case1-arg ...) #'(usage1 ...))
   (define (id case2-arg ... [bonus-arg default])
     (~@ NEWLINE body) ...)])


(define function-definition-shortcuts
  (refactoring-suite
   #:name (name function-definition-shortcuts)
   #:rules (list define-lambda-to-define
                 define-case-lambda-to-define)))
