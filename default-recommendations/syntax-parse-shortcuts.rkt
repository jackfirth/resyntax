#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-parse-shortcuts refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule define-syntax-syntax-parse-to-define-syntax-parse-rule
  #:description
  "This `define-syntax` macro with a single `syntax-parse` clause can be replaced with a simpler,
equivalent `define-syntax-parse-rule` macro."
  #:literals (define-syntax lambda)
  
  (define-syntax macro:id
    (lambda (stx-var:id)
      (syntax-parse-id:id parsed-stx:id
        [(_ . pattern) body ...])))
  
  #:when (and (free-identifier=? (attribute stx-var) (attribute parsed-stx))
              (equal? (syntax-e (attribute syntax-parse-id)) 'syntax-parse))
  
  (define-syntax-parse-rule (macro . pattern) body ...))


(define-refactoring-rule define-syntax-parser-to-define-syntax-parse-rule-simple
  #:description
  "This `define-syntax-parser` macro with a single clause can be replaced with a simpler, equivalent
`define-syntax-parse-rule` macro."
  #:literals (define-syntax-parser)
  
  (define-syntax-parser macro:id
    [(_ . pattern) body ...])
  
  (define-syntax-parse-rule (macro . pattern) body ...))


(define-refactoring-suite syntax-parse-shortcuts
  #:rules (define-syntax-syntax-parse-to-define-syntax-parse-rule
           define-syntax-parser-to-define-syntax-parse-rule-simple))
