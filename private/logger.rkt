#lang racket/base

(provide log-resyntax-fatal
         log-resyntax-error
         log-resyntax-warning
         log-resyntax-info
         log-resyntax-debug
         log-resyntax-rule-condition
         resyntax-logger)

(require (for-syntax racket/base)
         syntax/parse/define)

;@----------------------------------------------------------------------------------------------------

(define-logger resyntax)

(define (log-resyntax-rule-condition-impl v #:line line-num #:datum datum)
  (unless v
    (log-resyntax-debug "rule condition ~a on line ~a failed" datum line-num))
  v)

(define-syntax-parse-rule (log-resyntax-rule-condition expr:expr)
  #:with line (syntax-line (attribute expr))
  (log-resyntax-rule-condition-impl expr #:line 'line #:datum 'expr))
