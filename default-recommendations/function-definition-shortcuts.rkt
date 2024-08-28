#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [function-definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         guard
         racket/list
         rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/syntax-lines
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define/guard (free-identifiers=? ids other-ids)
  (define id-list (syntax->list ids))
  (define other-id-list (syntax->list other-ids))
  (guard (equal? (length id-list) (length other-id-list)) #:else #false)
  (for/and ([id (in-list id-list)] [other-id (in-list other-id-list)])
    (free-identifier=? id other-id)))


(define-syntax-class lambda-header
  (pattern _:id)
  (pattern (formal ...))
  (pattern (formal ... . rest-arg:id)))


(define-syntax-class possibly-nested-lambdas
  #:attributes ([argument-lists 1] [body 1])

  (pattern (_:lambda-by-any-name first-argument-list:lambda-header nested:possibly-nested-lambdas)
    #:with (argument-lists ...) (cons #'first-argument-list (attribute nested.argument-lists))
    #:with (body ...) #'(nested.body ...))

  (pattern
      (_:lambda-by-any-name
       first-argument-list:lambda-header
       (~and initial-body (~not _:possibly-nested-lambdas))
       remaining-body ...)
    #:with (argument-lists ...) #'(first-argument-list)
    #:with (body ...)
    #'((ORIGINAL-GAP first-argument-list initial-body) initial-body remaining-body ...)))


(define/guard (build-function-header original-header converted-lambda-formal-lists)
  (guard-match (cons first-formals remaining-formals) converted-lambda-formal-lists #:else
    original-header)
  (with-syntax ([formals first-formals])
    (build-function-header #`(#,original-header . formals) remaining-formals)))


(define-refactoring-rule define-lambda-to-define
  #:description
  "The `define` form supports a shorthand for defining functions (including function-returning\
 functions)."
  #:literals (define)
  [(define header lambda-form:possibly-nested-lambdas)
   #:when (not (syntax-property this-syntax 'class-body))
   #:do [(define multiline-lambda-header-count
           (count multiline-syntax? (attribute lambda-form.argument-lists)))]
   #:when (< multiline-lambda-header-count 2)
   #:when (oneline-syntax? #'header)
   #:when (or (identifier? #'header) (zero? multiline-lambda-header-count))
   #:with new-header (build-function-header #'header (attribute lambda-form.argument-lists))
   (define new-header lambda-form.body ...)])


(define-refactoring-rule define-case-lambda-to-define
  #:description "This use of `case-lambda` is equivalent to using `define` with optional arguments."
  #:literals (define case-lambda)
  [(define id:id
     (case-lambda
       [(case1-arg:id ...)
        (usage:id usage1:id ... default:expr)]
       [(case2-arg:id ... bonus-arg:id)
        body ...]))
   #:when (oneline-syntax? #'default)
   #:when (free-identifier=? #'id #'usage)
   #:when (free-identifiers=? #'(case1-arg ...) #'(case2-arg ...))
   #:when (free-identifiers=? #'(case1-arg ...) #'(usage1 ...))
   (define (id case2-arg ... [bonus-arg default])
     body ...)])


(define function-definition-shortcuts
  (refactoring-suite
   #:name (name function-definition-shortcuts)
   #:rules (list define-lambda-to-define
                 define-case-lambda-to-define)))
