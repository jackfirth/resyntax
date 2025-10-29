#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [cond-let-replacement refactoring-suite?]))


(require racket/list
         resyntax/base
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/default-recommendations/private/if-arm
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class let-refactorable-cond-clause
  #:attributes (refactored)
  (pattern [condition:expr leading-body ... let-expr:refactorable-let-expression]
    #:with refactored
    #`(~replacement
       [condition leading-body ... (~@ . (~focus-replacement-on (let-expr.refactored ...)))]
       #:original #,this-syntax)))


(define-refactoring-rule cond-let-to-cond-define
  #:description
  "Internal definitions are recommended instead of `let` expressions, to reduce nesting."
  #:literals (cond)
  (cond-id:cond clause-before ... clause:let-refactorable-cond-clause clause-after ...)
  (cond-id clause-before ... clause.refactored clause-after ...))


(define-refactoring-rule if-let-to-cond
  #:description
  "`cond` with internal definitions is preferred over `if` with `let`, to reduce nesting"
  #:literals (if void)
  (if condition
      (~and then-expr:if-arm (~not (void)))
      (~and else-expr:if-arm (~not (void))))
  #:when (or (attribute then-expr.uses-let?) (attribute else-expr.uses-let?))
  (cond (~replacement [condition then-expr.refactored ...] #:original-splice (condition then-expr))
        (~replacement [else else-expr.refactored ...] #:original else-expr)))


(define-refactoring-rule and-let-to-cond
  #:description
  "Using `cond` allows converting `let` to internal definitions, reducing nesting"
  #:literals (and cond)
  (and condition let-expr:refactorable-let-expression)
  #:when (not (empty? (attribute let-expr.id)))
  (cond [condition let-expr.refactored ...]
        [else #false]))


(define-refactoring-suite cond-let-replacement
  #:rules (and-let-to-cond
           cond-let-to-cond-define
           if-let-to-cond))
