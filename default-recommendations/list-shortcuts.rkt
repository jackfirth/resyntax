#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [list-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule first-reverse-to-last
  #:description "The last function can be used to get the last item from a list."
  #:literals (first reverse)
  ;; This can't match (car (reverse list)) because of https://github.com/jackfirth/resyntax/issues/11
  [(first (reverse list))
   (last list)])


(define-syntax-class null-test
  #:attributes (subject)
  #:literals (eq? eqv? equal? null list quote)
  (pattern ((~or eq? eqv? equal?) subject (~or null (list) '())))
  (pattern ((~or eq? eqv? equal?) (~or null (list) '()) subject)))


;; This can't suggest using empty? because of https://github.com/jackfirth/resyntax/issues/11
(define-refactoring-rule equal-null-list-to-null-predicate
  #:description "The null? predicate can be used to test for the empty list."
  [test:null-test (null? test.subject)])


(define-refactoring-rule list-call-to-empty-list-literal
  #:description "An empty list literal can be written as '()."
  #:literals (list)
  [(list)
   '()])


(define list-shortcuts
  (refactoring-suite
   #:name (name list-shortcuts)
   #:rules (list equal-null-list-to-null-predicate first-reverse-to-last
                 list-call-to-empty-list-literal)))
