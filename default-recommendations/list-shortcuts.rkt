#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [list-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/set
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/literal-constant
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/private/syntax-neighbors
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule first-reverse-to-last
  #:description "The `last` function can be used to get the last item from a list."
  #:literals (first reverse)
  ;; This can't match (car (reverse list)) because of https://github.com/jackfirth/resyntax/issues/11
  (first (reverse list))
  (last list))


(define-syntax-class null-test
  #:attributes (subject)
  #:literals (eq? eqv? equal? null list quote)
  (pattern ((~or eq? eqv? equal?) subject (~or null (list) '())))
  (pattern ((~or eq? eqv? equal?) (~or null (list) '()) subject)))


;; This can't suggest using empty? because of https://github.com/jackfirth/resyntax/issues/11
(define-refactoring-rule equal-null-list-to-null-predicate
  #:description "The `null?` predicate can be used to test for the empty list."
  test:null-test
  (null? test.subject))


(define-refactoring-rule append*-and-map-to-append-map
  #:description
  "The `append-map` function can be used to map each element into multiple elements in a single pass."
  #:literals (append* map)
  (append* (map f lst))
  (append-map f lst))


(define-refactoring-rule append-single-list-to-single-list
  #:description "The `append` function does nothing when applied to only one list."
  #:literals (append)
  (append lst)
  lst)


(define-refactoring-rule filter-to-remove*
  #:description
  "The `remove*` function is a simpler way to remove all elements of one list from another."
  #:literals (filter andmap not equal?)
  (filter (_:lambda-by-any-name (x1) (andmap (_:lambda-by-any-name (y1) (not (equal? x2 y2))) ys)) xs)
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (not (set-member? (syntax-free-identifiers #'ys) #'x1))
  (remove* ys xs))


(define-refactoring-rule filter-to-remv*
  #:description
  "The `remv*` function is a simpler way to remove all elements of one list from another."
  #:literals (filter andmap not eqv?)
  (filter (_:lambda-by-any-name (x1) (andmap (_:lambda-by-any-name (y1) (not (eqv? x2 y2))) ys)) xs)
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (not (set-member? (syntax-free-identifiers #'ys) #'x1))
  (remv* ys xs))


(define-refactoring-rule filter-to-remq*
  #:description
  "The `remq*` function is a simpler way to remove all elements of one list from another."
  #:literals (filter andmap not eq?)
  (filter (_:lambda-by-any-name (x1) (andmap (_:lambda-by-any-name (y1) (not (eq? x2 y2))) ys)) xs)
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (not (set-member? (syntax-free-identifiers #'ys) #'x1))
  (remq* ys xs))


(define-refactoring-rule sort-with-keyed-comparator-to-sort-by-key
  #:description "This `sort` expression can be replaced with a simpler, equivalent expression."
  #:literals (sort <)
  (sort lst (_:lambda-by-any-name (x1:id y1:id) (less-than:id (f1:id x2:id) (f2:id y2:id))))
  #:when (free-identifier=? #'x1 #'x2)
  #:when (free-identifier=? #'y1 #'y2)
  #:when (free-identifier=? #'f1 #'f2)
  (sort lst less-than #:key f1))


(define-syntax-class unquoted
  #:attributes (expr)
  #:literals (unquote)
  (pattern expr:literal-constant)
  (pattern (unquote expr)))


(define-refactoring-rule quasiquote-to-list
  #:description "This quasiquotation is equialent to a simple `list` call."
  #:literals (quasiquote unquote)
  (quasiquote (arg:unquoted ...))
  (list arg.expr ...))


(define-definition-context-refactoring-rule ignored-map-to-for-each
  #:description "The result of this `map` expression is unused. Consider using `for-each` instead."
  #:literals (map)
  (~seq body-before ... (~and map-expr (map proc list ...)) body-after ...+)
  (body-before ... (~replacement (for-each proc list ...) #:original map-expr) body-after ...))


(define-refactoring-suite list-shortcuts
  #:rules (append-single-list-to-single-list
           append*-and-map-to-append-map
           equal-null-list-to-null-predicate
           filter-to-remove*
           filter-to-remq*
           filter-to-remv*
           first-reverse-to-last
           ignored-map-to-for-each
           quasiquote-to-list
           sort-with-keyed-comparator-to-sort-by-key))
