#lang racket/base


(provide if-arm)


(require resyntax/base
         resyntax/default-recommendations/let-replacement/private/let-binding
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class if-arm
  #:attributes (uses-begin? uses-let? [refactored 1])
  #:literals (begin)

  (pattern (begin body ...)
    #:attr uses-begin? #true
    #:attr uses-let? #false
    #:with (refactored ...) #`(~splicing-replacement (body ...) #:original #,this-syntax))

  (pattern :refactorable-let-expression
    #:attr uses-begin? #false
    #:attr uses-let? #true)

  (pattern other
    #:with (refactored ...) #'(other)
    #:attr uses-begin? #false
    #:attr uses-let? #false))
