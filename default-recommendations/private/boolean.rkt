#lang racket/base


(provide condition-expression
         known-false
         known-not-false
         likely-boolean)


(require racket/string
         resyntax/default-recommendations/private/literal-constant
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class known-false
  (pattern constant:literal-constant #:when (not (attribute constant.value))))


(define-syntax-class known-not-false
  (pattern constant:literal-constant #:when (attribute constant.value)))


(define-syntax-class known-true
  (pattern constant:literal-constant #:when (equal? (attribute constant.value) #true)))


(define-syntax-class likely-boolean
  #:literals (or and not if)
  (pattern true:known-true)
  (pattern false:known-false)
  (pattern (or _:likely-boolean ...))
  (pattern (and _ ... _:likely-boolean))
  (pattern (not _))
  (pattern (f:likely-boolean-returning arg ...))
  (pattern (if _ _:likely-boolean _:likely-boolean)))


(define-syntax-class likely-boolean-returning
  #:literals (= < > <= >=)
  (pattern id:id #:when (string-suffix? (symbol->string (syntax-e #'id)) "?"))
  (pattern (~or = < > <= >=)))


(define-syntax-class condition-expression
  #:attributes (negated? base-condition)
  #:literals (not)
  (pattern (not base-condition:expr) #:with negated? #true)
  (pattern base-condition:expr #:with negated? #false))
