#lang racket/base


(provide self-quoting-literal)


(require syntax/parse)


;@----------------------------------------------------------------------------------------------------


;; A datum that evaluates to itself, so it means the same thing in expression position as it does
;; within quoted or quasiquoted data. Symbols and quoted datums like (quote x) are notably not
;; self-quoting: a symbol in expression position is a variable reference, and within quasiquoted
;; data (quote x) is a two-element list rather than the value x.
(define-syntax-class self-quoting-literal
  (pattern (~or _:boolean _:character _:number _:regexp _:byte-regexp _:string _:bytes)))
