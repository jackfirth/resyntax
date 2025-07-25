#lang racket/base


(provide pure-expression)


(require resyntax/default-recommendations/private/literal-constant
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class pure-list-accessor
  #:literals (car cdr cadr cdar caar cddr caddr cadddr)
  (pattern (~or car cdr cadr cdar caar cddr caddr cadddr)))


(define-syntax-class pure-expression
  (pattern (~or _:literal-constant 
                _:id
                (_:pure-list-accessor arg:pure-expression))))
