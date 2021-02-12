#lang racket/base


(provide lambda-by-any-name)


(require syntax/parse)


;@----------------------------------------------------------------------------------------------------


;; λ and lambda aren't free-identifier=?. Additionally, by using a syntax class instead of #:literals
;; we can produce the same lambda identifier that the input syntax had instead of changing all lambda
;; identfiers to one of the two cases. There doesn't seem to be a strong community consensus on which
;; name should be used, so we want to avoid changing the original code's choice.
(define-syntax-class lambda-by-any-name
  #:literals (λ lambda)
  (pattern (~or λ lambda)))
