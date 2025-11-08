#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-parse-shortcuts refactoring-suite?]))


(require (for-syntax racket/base
                     syntax/parse)
         rebellion/private/static-name
         resyntax/base
         resyntax/private/more-syntax-parse-classes
         resyntax/private/syntax-traversal
         racket/stream
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule define-syntax-syntax-parse-to-define-syntax-parse-rule
  #:description
  "This `define-syntax` macro with a single `syntax-parse` clause can be replaced with a simpler,
equivalent `define-syntax-parse-rule` macro."
  #:literals (define-syntax lambda [syntax-parse syntax-parse #:phase 1] [syntax-id syntax #:phase 1])
  
  (define-syntax macro:id
    (lambda (stx-id:id)
      (syntax-parse stx-id2:id
        [(_ pattern ...) directive:syntax-parse-pattern-directive ... (syntax-id last-form)])))
  
  #:when (free-identifier=? (attribute stx-id) (attribute stx-id2) 1)

  #:with (new-body ...)
  (syntax-traverse #'((~@ . directive) ... last-form)
    [id-in-body:id
     #:when (free-identifier=? (attribute id-in-body) (attribute stx-id) 1)
     (syntax-property #'this-syntax 'skip-incorrect-binding-check? #true)])
  
  (define-syntax-parse-rule (macro pattern ...) new-body ...))


(define-refactoring-rule define-syntax-parser-to-define-syntax-parse-rule-simple
  #:description
  "This `define-syntax-parser` macro with a single clause can be replaced with a simpler, equivalent
`define-syntax-parse-rule` macro."
  #:literals (define-syntax-parser)
  
  (define-syntax-parser macro:id
    [(_ . pattern) body ...])
  
  #:do [(define (strip-syntax-wrapper stx)
          (syntax-parse stx
            #:literals (syntax)
            [(syntax body) #'body]
            [other #'other]))
        (define new-body (map strip-syntax-wrapper (attribute body)))]
  
  #:with (new-body-part ...) new-body
  
  (define-syntax-parse-rule (macro . pattern) new-body-part ...))


(define-refactoring-suite syntax-parse-shortcuts
  #:rules (define-syntax-syntax-parse-to-define-syntax-parse-rule
           define-syntax-parser-to-define-syntax-parse-rule-simple))
