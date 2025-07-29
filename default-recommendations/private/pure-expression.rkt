#lang racket/base

(provide pure-expression)

(require racket/list
         resyntax/default-recommendations/private/literal-constant
         syntax/parse)

;@----------------------------------------------------------------------------------------------------

(define-literal-set
 pure-functions
 (list-ref hash-ref first second third fourth fifth car cdr cadr cdar caar cddr caddr cadddr))

(define-syntax-class pure-expression
  (pattern :literal-constant)
  (pattern :id)
  (pattern (f:id arg:pure-expression ...)
    #:when ((literal-set->predicate pure-functions) (attribute f))))
