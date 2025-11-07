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
  
  #:do [(define stx-id (attribute stx-var))
        ;; Check if stx-var is referenced in the body
        (define (has-stx-ref? stx)
          (syntax-parse stx
            [id:id
             #:when (free-identifier=? #'id stx-id)
             #t]
            [(head . tail)
             (or (has-stx-ref? #'head)
                 (has-stx-ref? #'tail))]
            [_ #f]))
        (define body-has-stx-ref?
          (ormap has-stx-ref? (attribute body)))]
  
  #:when (and (free-identifier=? (attribute stx-var) (attribute parsed-stx))
              (equal? (syntax-e (attribute syntax-parse-id)) 'syntax-parse)
              (not body-has-stx-ref?))
  
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
