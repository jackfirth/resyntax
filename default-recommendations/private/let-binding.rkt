#lang racket/base


(require racket/contract/base)


(provide
 body-with-refactorable-let-expression
 refactorable-let-expression
 (contract-out
  [identifier-binding-unchanged-in-context? (-> identifier? syntax? boolean?)]
  [identifier-has-exact-binding-in-context? (-> identifier? syntax? boolean?)]))


(require racket/list
         racket/match
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/private/logger
         syntax/id-set
         syntax/parse
         syntax/parse/lib/function-header)


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class body-with-refactorable-let-expression
  #:attributes ([refactored 1] [id 1])
  (pattern
    (~seq leading-body ... let-expression:refactorable-let-expression)
    #:with (refactored ...) #'(leading-body ... let-expression.refactored ...)
    #:with (id ...) (attribute let-expression.id)))


(define-syntax-class refactorable-let-expression
  #:attributes ([refactored 1] [id 1])
  (pattern (header:let-header body:let-body)
    #:with (id ...) (append (attribute header.id) (attribute body.id))
    #:when (for/and ([id (attribute id)]
                     #:unless (unused-id? id))
             (not (identifier-has-exact-binding-in-context? id this-syntax)))
    #:when (not (check-duplicate-identifier
                 (for/list ([id (attribute id)]
                            #:unless (unused-id? id))
                   (identifier-in-context id this-syntax))))
    #:when (for/and ([id (attribute header.id)]
                     #:unless (unused-id? id))
             (identifier-binding-unchanged-in-context? id (attribute body.first-body)))
    #:with (refactored ...)
    #`(~splicing-replacement (header.definition ... body.refactored ...) #:original #,this-syntax)))


(define-splicing-syntax-class let-body
  #:attributes (first-body [refactored 1] [id 1])
  (pattern :body-with-refactorable-let-expression
    #:with first-body (first (attribute refactored)))
  (pattern (~seq first-body body ...)
    #:with (refactored ...) this-syntax
    #:with (id ...) '()))
  

(define-splicing-syntax-class let-header
  #:attributes ([id 1] [definition 1])
  #:literals (let let-values let* let*-values)
  (pattern (~seq (~or let let-values) ~! :binding-group))
  (pattern (~seq (~or let* let*-values) ~! (~var bindings (binding-group #:nested? #true)))
    #:attr [id 1] (attribute bindings.id)
    #:attr [definition 1] (attribute bindings.definition)))


(define-syntax-class binding-clause
  #:attributes ([id 1] rhs definition)

  (pattern [all-ids:id-list rhs:expr]
    #:do [(log-resyntax-debug
           "refactorable-let-expression: checking binding-clause not self-shadowing: ~a"
           this-syntax)]
    #:when (for*/and ([rhs-free-id (in-free-id-set (syntax-free-identifiers (attribute rhs)))]
                      [id (in-list (attribute all-ids.id))])
             (log-resyntax-debug
              "refactorable-let-expression: checking identifier ~a not shadowed by ~a"
              rhs-free-id id)
             (identifier-would-self-shadow? id rhs-free-id #:full-right-hand-side (attribute rhs)))
    #:cut
    #:with (id ...) (attribute all-ids.id)
    #:with definition
    (match (attribute id)
      [(list (? unused-id?) ...) #'rhs]
      [(list only-id)
       (syntax-parse (attribute rhs)
         [(_:lambda-by-any-name (arg:formal ...) body ...)
          #`(define (#,only-id (~@ . arg) ...)
              body ...)]
         [(_:lambda-by-any-name (arg:formal ...+ . tail-arg) body ...)
          #`(define (#,only-id (~@ . arg) ... . tail-arg)
              body ...)]
         [(_:lambda-by-any-name tail-arg:identifier body ...)
          #`(define (#,only-id . tail-arg)
              body ...)]
         [_
          #`(define (~replacement #,only-id #:original all-ids) rhs)])]
      [_ #'(define-values all-ids rhs)])))


(define-syntax-class id-list
  #:attributes ([id 1])
  (pattern only-id:id #:with (id ...) (list (attribute only-id)))
  (pattern (id ...)))


(define-syntax-class (binding-group #:nested? [nested #false])
  #:attributes ([id 1] [definition 1])
  (pattern (clause:binding-clause ...)
    #:when (or (not nested)
               (for/and ([rhs (in-list (attribute clause.rhs))]
                         [i (in-naturals)]
                         #:when #true
                         [ids (in-list (attribute clause.id))]
                         [j (in-naturals)]
                         #:when (< i j)
                         [id (in-list ids)]
                         #:when #true
                         [rhs-free-id (in-free-id-set (syntax-free-identifiers rhs))])
                 (identifier-binding-unchanged-in-context? rhs-free-id id)))
    #:cut
    #:with (id ...) #'(clause.id ... ...)
    #:with (definition ...)
    #`(~splicing-replacement ((~replacement clause.definition #:original clause) ...)
                             #:original #,this-syntax)))


(define (unused-id? id)
  (empty? (or (syntax-property id 'identifier-usages) '())))


(define (identifier-would-self-shadow? id rhs-id #:full-right-hand-side rhs)
  (or (identifier-binding-unchanged-in-context? rhs-id id)
      (not (free-identifier=? rhs-id (identifier-in-context rhs-id rhs)))))


(define (identifier-binding-unchanged-in-context? id context)
  (define add-context (make-syntax-delta-introducer context #false))
  (free-identifier=? id (add-context id)))


(define (identifier-has-exact-binding-in-context? id context)
  (and (identifier-binding (identifier-in-context id context) 0 #false #true) #true))


(define (identifier-in-context id context)
  (datum->syntax context (syntax-e id) id id))
