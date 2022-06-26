#lang racket/base


(require racket/contract)


(provide
 (contract-out
  [legacy-contract-migrations refactoring-suite?]))


(require (for-syntax racket/base)
         racket/syntax
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule false/c-migration
  #:description "`false/c` is an alias for `#f` that exists for backwards compatibility."
  #:literals (false/c)
  [false/c
   #false])


(define-refactoring-rule symbols-migration
  #:description "`symbols` is equivalent to `or/c` and exists for backwards compatibility."
  #:literals (symbols)
  [(symbols sym ...)
   (or/c sym ...)])


(define-refactoring-rule vector-immutableof-migration
  #:description "`vector-immutableof` is a legacy form that is equivalent to `vectorof` with the\
 `#:immutable` option"
  #:literals (vector-immutableof)
  [(vector-immutableof c)
   (vectorof c #:immutable #true)])


(define-refactoring-rule vector-immutable/c-migration
  #:description "`vector-immutable/c` is a legacy form that is equivalent to `vector/c` with the\
 `#:immutable` option"
  #:literals (vector-immutable/c)
  [(vector-immutable/c c ...)
   (vector/c c ... #:immutable #true)])


(define-refactoring-rule box-immutable/c-migration
  #:description "`box-immutable/c` is a legacy form that is equivalent to `box/c` with the `#:immutable`\
 option"
  #:literals (box-immutable/c)
  [(box-immutable/c c)
   (box/c c #:immutable #true)])


(define-refactoring-rule flat-contract-migration
  #:description "flat-contract is a legacy form for constructing contracts from predicates;\
 predicates can be used directly as contracts now."
  #:literals (flat-contract)
  [(flat-contract predicate)
   predicate])


(define-refactoring-rule contract-struct-migration
  #:description "The `contract-struct` form is deprecated; use `struct` instead. Lazy struct contracts no\
 longer require a separate struct declaration."
  #:literals (contract-struct)
  [(contract-struct id fields)
   (struct id fields)])


(define-refactoring-rule define-contract-struct-migration
  #:description "The `define-contract-struct` form is deprecated, use `struct` instead. Lazy struct\
 contracts no longer require a separate struct declaration."
  #:literals (define-contract-struct)
  [(define-contract-struct id fields)
   #:with make-id (format-id #'id "make-~a" #'id)
   (struct id fields #:extra-constructor-name make-id)])


(define legacy-contract-migrations
  (refactoring-suite
   #:name (name legacy-contract-migrations)
   #:rules
   (list box-immutable/c-migration
         contract-struct-migration
         define-contract-struct-migration
         false/c-migration
         flat-contract-migration
         symbols-migration
         vector-immutableof-migration
         vector-immutable/c-migration)))
