#lang racket/base


(provide always-throwing-expression)


(require racket/contract/combinator
         racket/generic
         syntax/parse
         syntax/readerr)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class always-throwing-expression
  #:literals (raise
              error
              raise-user-error
              raise-argument-error
              raise-result-error
              raise-arguments-error
              raise-range-error
              raise-type-error
              raise-mismatch-error
              raise-arity-error
              raise-arity-mask-error
              raise-result-arity-error
              raise-syntax-error
              raise-read-error
              raise-read-eof-error
              raise-blame-error
              raise-support-error)
  (pattern ((~or raise
              error
              raise-user-error
              raise-argument-error
              raise-result-error
              raise-arguments-error
              raise-range-error
              raise-type-error
              raise-mismatch-error
              raise-arity-error
              raise-arity-mask-error
              raise-result-arity-error
              raise-syntax-error
              raise-read-error
              raise-read-eof-error
              raise-blame-error
              raise-support-error)
            arg ...)))
              