#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [legacy-struct-migrations (listof refactoring-rule?)]))


(require (for-syntax racket/base)
         racket/syntax
         resyntax/refactoring-rule
         resyntax/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class id-maybe-super
  #:attributes (id super-id)
  (pattern id:id #:attr super-id #false)
  (pattern (id:id super-id:id)))


(define-syntax-class constructor-name-keyword
  (pattern (~or #:constructor-name #:extra-constructor-name)))


(define-splicing-syntax-class struct-option
  #:attributes ([formatted 1])
  (pattern (~seq (~and keyword:keyword (~not :constructor-name-keyword)) expr:expr ...)
    #:with (formatted ...) #'(keyword expr ...)))


(define-refactoring-rule define-struct-to-struct
  #:description "The define-struct form exists for backwards compatibility, struct is preferred."
  #:literals (define-struct)
  [(define-struct id-maybe-super:id-maybe-super fields
     option:struct-option ...)
   #:with make-id (format-id #'id-maybe-super.id "make-~a" #'id-maybe-super.id)
   (struct id-maybe-super.id (~? id-maybe-super.super-id) fields
     NEWLINE #:extra-constructor-name make-id
     (~@ NEWLINE option.formatted ...) ...)])


(define legacy-struct-migrations
  (list define-struct-to-struct))
