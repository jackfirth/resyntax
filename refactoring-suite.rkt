#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactoring-suite? predicate/c]
  [refactoring-suite
   (->*
    () (#:rules (sequence/c refactoring-rule?) #:name (or/c interned-symbol? #false))
    refactoring-suite?)]
  [refactoring-suite-rules (-> refactoring-suite? (listof refactoring-rule?))]))


(require racket/sequence
         rebellion/base/symbol
         rebellion/type/object
         resyntax/refactoring-rule)


;@----------------------------------------------------------------------------------------------------


(define-object-type refactoring-suite (rules)
  #:constructor-name constructor:refactoring-suite
  #:omit-root-binding)


(define (refactoring-suite #:rules [rules '()] #:name [name #false])
  (constructor:refactoring-suite #:rules (sequence->list rules) #:name name))
