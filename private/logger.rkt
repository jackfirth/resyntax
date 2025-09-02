#lang racket/base


(provide log-resyntax-fatal
         log-resyntax-error
         log-resyntax-warning
         log-resyntax-info
         log-resyntax-debug
         log-resyntax-rule-condition
         log-resyntax-rule-with-expr
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


(define-syntax-parse-rule (log-resyntax-rule-with-expr expr:expr)
  #:with line (syntax-line (attribute expr))
  (begin
    (define temp-result 
      (with-handlers ([exn:fail? (λ (e) 
                                   (log-resyntax-debug "rule #:with expression ~a failed with error: ~a on line ~a" 
                                                       'expr (exn-message e) 'line)
                                   (raise e))])
        expr))
    temp-result))


(define-syntax-parse-rule (log-resyntax-rule-undo rule-name:id)
  (log-resyntax-debug "~a: rule match discarded (undo)" 'rule-name))
