#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [cond-let-replacement refactoring-suite?]))


(require resyntax/base
         resyntax/default-recommendations/let-replacement/private/let-binding
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


(define-refactoring-suite cond-let-replacement
  #:rules (cond-let-to-cond-define))
