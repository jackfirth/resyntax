#lang racket/base


(provide pure-expression)


(require resyntax/default-recommendations/private/literal-constant
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class pure-expression
  (pattern (~or _:literal-constant _:id)))
