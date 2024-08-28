#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [legacy-struct-migrations refactoring-suite?]))


(require (for-syntax racket/base)
         racket/syntax
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class id-maybe-super
  #:attributes (make-id [migrated 1])

  (pattern id:id
    #:with (migrated ...) #'(id)
    #:with make-id (format-id #'id "make-~a" #'id))
  
  (pattern (id:id super-id:id)
    #:with (migrated ...) #'(id super-id)
    #:with make-id (format-id #'id "make-~a" #'id)))


(define-syntax-class constructor-name-keyword
  (pattern (~or #:constructor-name #:extra-constructor-name)))


(define-splicing-syntax-class struct-option
  #:attributes (keyword [expr 1] [original 1])
  (pattern (~seq (~and keyword:keyword (~not :constructor-name-keyword)) expr:expr ...)
    #:with (original ...) #'(keyword expr ...)))


(define-refactoring-rule define-struct-to-struct
  #:description "The `define-struct` form exists for backwards compatibility, `struct` is preferred."
  #:literals (define-struct)
  [(define-struct id:id-maybe-super fields option:struct-option ...)
   (struct id.migrated ... fields option.original ... ...
     #:extra-constructor-name id.make-id)])


(define legacy-struct-migrations
  (refactoring-suite #:name (name legacy-struct-migrations) #:rules (list define-struct-to-struct)))
