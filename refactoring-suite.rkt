#lang racket/base


(require racket/contract/base)


(provide
 define-refactoring-suite
 (contract-out
  [refactoring-suite? predicate/c]
  [refactoring-suite
   (->* ()
        (#:rules (sequence/c refactoring-rule?) #:name (or/c interned-symbol? #false))
        refactoring-suite?)]
  [refactoring-suite-rules (-> refactoring-suite? (listof refactoring-rule?))]))


(require (for-syntax racket/base)
         racket/sequence
         rebellion/base/symbol
         rebellion/type/object
         resyntax/refactoring-rule
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-object-type refactoring-suite (rules)
  #:constructor-name constructor:refactoring-suite
  #:omit-root-binding)


(define (refactoring-suite #:rules [rules '()] #:name [name #false])
  (constructor:refactoring-suite #:rules (sequence->list rules) #:name name))


(begin-for-syntax

  (define-splicing-syntax-class rules-list
    #:attributes (as-list-expr)
    (pattern (~seq) #:with as-list-expr #'(list))
    (pattern (~seq #:rules ~! (rule ...))
      #:declare rule (expr/c #'refactoring-rule?)
      #:with as-list-expr #'(list rule.c ...)))

  (define-splicing-syntax-class suites-list
    #:attributes (as-list-expr)
    (pattern (~seq) #:with as-list-expr #'(list))
    (pattern (~seq #:suites ~! (suite ...))
      #:declare suite (expr/c #'refactoring-suite?)
      #:with as-list-expr #'(append (refactoring-suite-rules suite.c) ...))))


(define-syntax-parse-rule (define-refactoring-suite id:id rules:rules-list suites:suites-list)
  (define id
    (refactoring-suite
     #:name 'id
     #:rules (append rules.as-list-expr suites.as-list-expr))))
