#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [dict-suggestions refactoring-suite?]))


(require racket/dict
         racket/set
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-literal-set simple-for-loops
  (for
      for*
    for/list
    for*/list
    for/vector
    for*/vector
    for/set
    for*/set
    for/sum
    for*/sum
    for/product
    for*/product
    for/and
    for*/and
    for/or
    for*/or
    for/first
    for*/first
    for/last
    for*/last
    for/hash
    for*/hash))


(define-refactoring-rule in-dict-to-in-dict-keys
  #:description "This `in-dict` can be replaced with `in-dict-keys` since the value is not used."
  #:literals (in-dict)
  (for-id:id (clause-before ... [(key:id value:id) (in-dict dict-expr)] clause-after ...) body ...)
  #:when ((literal-set->predicate simple-for-loops) (attribute for-id))
  #:when (equal? (syntax-property #'value 'usage-count) 0)
  (for-id (clause-before ... [key (in-dict-keys dict-expr)] clause-after ...) body ...))


(define-refactoring-rule in-dict-to-in-dict-values
  #:description "This `in-dict` can be replaced with `in-dict-values` since the key is not used."
  #:literals (in-dict)
  (for-id:id (clause-before ... [(key:id value:id) (in-dict dict-expr)] clause-after ...) body ...)
  #:when ((literal-set->predicate simple-for-loops) (attribute for-id))
  #:when (equal? (syntax-property #'key 'usage-count) 0)
  (for-id (clause-before ... [value (in-dict-values dict-expr)] clause-after ...) body ...))


(define-refactoring-suite dict-suggestions
  #:rules (in-dict-to-in-dict-keys
           in-dict-to-in-dict-values))
