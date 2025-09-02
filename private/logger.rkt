#lang racket/base


(provide log-resyntax-fatal
         log-resyntax-error
         log-resyntax-warning
         log-resyntax-info
         log-resyntax-debug
         log-resyntax-rule-condition
         log-resyntax-rule-with-pattern
         log-resyntax-rule-undo
         resyntax-logger)


(require (for-syntax racket/base)
         rebellion/base/option
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


(define (log-resyntax-rule-with-pattern-impl pattern-result #:line line-num #:pattern-datum pattern-datum #:expr-datum expr-datum)
  (when (absent? pattern-result)
    (log-resyntax-debug "rule #:with pattern ~a failed to match ~a on line ~a" pattern-datum expr-datum line-num))
  pattern-result)


(define-syntax-parse-rule (log-resyntax-rule-with-pattern pattern-expr:expr expr:expr)
  #:with line (syntax-line (attribute expr))
  (log-resyntax-rule-with-pattern-impl 
   (syntax-parse expr #:track-literals [pattern-expr (present #'pattern-expr)] [_ absent])
   #:line 'line 
   #:pattern-datum 'pattern-expr 
   #:expr-datum 'expr))


(define-syntax-parse-rule (log-resyntax-rule-undo rule-name:id)
  (log-resyntax-debug "~a: rule match discarded (undo)" 'rule-name))
