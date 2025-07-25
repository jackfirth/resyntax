#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [function-definition-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         guard
         racket/list
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/list-function
         resyntax/default-recommendations/private/syntax-lines
         resyntax/private/logger
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
    #:with (body ...) #'(initial-body remaining-body ...)))


(define (zero-argument-formals? formals)
  (syntax-parse formals
    [() #true]
    [_ #false]))


(define/guard (build-function-header original-header formal-lists)
  (guard-match (cons first-formals remaining-formals) formal-lists #:else
    original-header)
  (guard (or (identifier? original-header) (not (zero-argument-formals? first-formals))) #:else
    original-header)
  (with-syntax ([formals first-formals])
    (build-function-header #`(#,original-header . formals) remaining-formals)))


(define/guard (build-function-body original-header formal-lists innermost-body-forms)
  (guard-match (cons first-formals remaining-formals) formal-lists #:else
    innermost-body-forms)
  (guard (or (identifier? original-header) (not (zero-argument-formals? first-formals))) #:else
    (build-lambda-expressions formal-lists innermost-body-forms))
  (with-syntax ([formals first-formals])
    (build-function-body #`(#,original-header . formals) remaining-formals innermost-body-forms)))


(define/guard (build-lambda-expressions formal-lists innermost-body-forms)
  (guard-match (cons first-formals remaining-formals) formal-lists #:else
    innermost-body-forms)
  (list #`(λ #,first-formals #,@(build-lambda-expressions remaining-formals innermost-body-forms))))


(define-refactoring-rule define-lambda-to-define
  #:description "The `define` form supports a shorthand for defining functions."
  #:literals (define)
  (define id:id lambda-form:possibly-nested-lambdas)
  #:when (not (syntax-property this-syntax 'class-body))
  #:do [(define multiline-lambda-header-count
          (count multiline-syntax? (attribute lambda-form.argument-lists)))]
  #:when (< multiline-lambda-header-count 2)
  #:with new-header (build-function-header #'id (attribute lambda-form.argument-lists))
  #:with (new-body ...)
  (build-function-body #'id (attribute lambda-form.argument-lists) (attribute lambda-form.body))
  (define new-header new-body ...))


(define-refactoring-rule define-lambda-to-curried-define
  #:description "Functions returning lambdas can be written using curried function syntax."
  #:literals (define)
  (define (~and header (~not :id)) lambda-form:possibly-nested-lambdas)
  #:when (not (syntax-property this-syntax 'class-body))
  #:do [(define multiline-lambda-header-count
          (count multiline-syntax? (attribute lambda-form.argument-lists)))]
  #:when (< multiline-lambda-header-count 2)
  #:when (oneline-syntax? #'header)
  #:when (zero? multiline-lambda-header-count)
  #:when (not (zero-argument-formals? (first (attribute lambda-form.argument-lists))))
  #:with new-header (build-function-header #'header (attribute lambda-form.argument-lists))
  #:with (new-body ...)
  (build-function-body #'header (attribute lambda-form.argument-lists) (attribute lambda-form.body))
  (define new-header new-body ...))


(define-refactoring-rule define-case-lambda-to-define
  #:description "This use of `case-lambda` is equivalent to using `define` with optional arguments."
  #:literals (define case-lambda)

  (define id:id
    (case-lambda
      [(case1-arg:id ...) (usage:id usage1:id ... default:expr)]
      [(case2-arg:id ... bonus-arg:id) body ...]))

  #:when (oneline-syntax? #'default)
  #:when (free-identifier=? #'id #'usage)

  #:when (and (equal? (length (attribute case1-arg))
                      (length (attribute case2-arg)))
              (equal? (length (attribute case1-arg))
                      (length (attribute usage1))))

  #:when (for/and ([case1-arg-id (in-list (attribute case1-arg))]
                   [case2-arg-id (in-list (attribute case2-arg))]
                   [usage1-id (in-list (attribute usage1))])
           (and (equal? (syntax-e case1-arg-id) (syntax-e case2-arg-id))
                (equal? (syntax-e case1-arg-id) (syntax-e usage1-id))))

  (define (id case2-arg ... [bonus-arg default])
    body ...))


(define-refactoring-rule empty-checked-rest-args-to-optional-arg
  #:description
  "This function definition uses rest arguments in a way equivalent to using an optional argument."
  #:literals (define if)

  (define (f arg ... . rest-args:id)
    (define optional-arg:id
      (if (:empty-predicate-by-any-name rest-args2:id)
          default-expr
          (:first-by-any-name rest-args3:id)))
    body ...)

  #:when (oneline-syntax? (attribute default-expr))
  #:when (free-identifier=? (attribute rest-args) (attribute rest-args2))
  #:when (free-identifier=? (attribute rest-args) (attribute rest-args3))
  #:when (equal? (length (syntax-property (attribute rest-args) 'identifier-usages)) 1)

  (define (f arg ... [optional-arg default-expr])
    body ...))


(define-refactoring-suite function-definition-shortcuts
  #:rules (define-lambda-to-define
            define-lambda-to-curried-define
            define-case-lambda-to-define
            empty-checked-rest-args-to-optional-arg))
