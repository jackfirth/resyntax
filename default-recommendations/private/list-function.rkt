#lang racket/base


(provide empty-list-by-any-name
         empty-predicate-by-any-name
         first-by-any-name
         rest-by-any-name)


(require racket/list
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class empty-list-by-any-name
  #:literals (quote null empty list)
  (pattern (~or '() null empty (list))))


(define-syntax-class empty-predicate-by-any-name
  #:literals (null? empty?)
  (pattern (~or null? empty?)))


(define-syntax-class first-by-any-name
  #:literals (car first)
  (pattern (~or car first)))


(define-syntax-class rest-by-any-name
  #:literals (cdr rest)
  (pattern (~or cdr rest)))
