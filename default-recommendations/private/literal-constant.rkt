#lang racket/base


(provide literal-constant)


(require syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class literal-constant
  #:attributes (value)
  #:literals (quote)

  (pattern
   (~or (quote literal)
        literal:boolean
        literal:character
        literal:number
        literal:regexp
        literal:byte-regexp
        literal:string
        literal:bytes)
   #:attr value (syntax->datum #'literal)))
