#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [keep-sorted-suggestions refactoring-suite?]))


(require racket/list
         resyntax/base
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


;; Convert an identifier syntax object to its string representation
(define (identifier->string id)
  (symbol->string (syntax-e id)))


;; Check if a list of identifiers is sorted alphabetically by their symbol names
(define (identifiers-sorted? ids)
  (define id-strings (map identifier->string ids))
  (equal? id-strings (sort id-strings string<?)))


;; Sort a list of syntax objects representing identifiers alphabetically
(define (sort-identifiers ids)
  (sort ids string<? #:key identifier->string))


;; Syntax class for matching a list expression with identifier elements
(define-syntax-class list-of-ids
  #:attributes (constructor [element 1] sorted?)
  (pattern (constructor:id element:id ...)
    #:attr sorted? (identifiers-sorted? (attribute element))))


(define-refactoring-rule resort-keep-sorted-list
  #:description "This list is marked with `keep-sorted` but its elements are not in sorted order."
  body:list-of-ids
  #:when (syntax-property this-syntax 'keep-sorted)
  #:when (not (attribute body.sorted?))
  #:with (sorted-element ...) (sort-identifiers (attribute body.element))
  (body.constructor sorted-element ...))


(define-refactoring-suite keep-sorted-suggestions
  #:rules (resort-keep-sorted-list))
