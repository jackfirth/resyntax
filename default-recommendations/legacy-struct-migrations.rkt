#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [legacy-struct-migrations (listof refactoring-rule?)]))


(require (for-syntax racket/base)
         racket/list
         racket/syntax
         resyntax/refactoring-rule
         resyntax/syntax-replacement
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
  #:attributes (keyword [expr 1] [formatted 1] [original 1])
  (pattern (~seq (~and keyword:keyword (~not :constructor-name-keyword)) expr:expr ...)
    #:with (original ...) #'(keyword expr ...)
    #:with (formatted ...) #'(NEWLINE (ORIGINAL-SPLICE keyword expr ...))))


(define-refactoring-rule define-struct-to-struct
  #:description "The define-struct form exists for backwards compatibility, struct is preferred."
  #:literals (define-struct)
  [(define-struct id:id-maybe-super fields option:struct-option ...)
   (struct id.migrated ... (ORIGINAL-SPLICE fields option.original ... ...)
     NEWLINE #:extra-constructor-name id.make-id)])


(define legacy-struct-migrations
  (list define-struct-to-struct))
