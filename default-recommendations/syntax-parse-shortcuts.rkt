#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-parse-shortcuts refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base
         resyntax/private/syntax-traversal
         racket/stream
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
        ;; Check if stx-var is referenced in the body using syntax-search
        (define body-has-stx-ref?
          (for/or ([body-part (in-list (attribute body))])
            (not (stream-empty? 
                  (syntax-search body-part
                    [id:id #:when (free-identifier=? #'id stx-id)
                     (stream this-syntax)])))))
        ;; Replace references to stx-var with this-syntax and strip syntax wrapper
        (define (replace-stx-with-this-syntax stx)
          (syntax-parse stx
            [id:id #:when (free-identifier=? #'id stx-id)
             (datum->syntax #'here 'this-syntax stx)]
            [(a . b)
             (datum->syntax stx
                            (cons (replace-stx-with-this-syntax #'a)
                                  (replace-stx-with-this-syntax #'b))
                            stx
                            stx)]
            [other #'other]))
        (define (strip-syntax-wrapper stx)
          (syntax-parse stx
            #:literals (syntax)
            [(syntax body) #'body]
            [other #'other]))
        (define new-body
          (for/list ([body-part (in-list (attribute body))])
            (strip-syntax-wrapper (replace-stx-with-this-syntax body-part))))]
  
  #:with (new-body-part ...) new-body
  
  #:when (and (free-identifier=? (attribute stx-var) (attribute parsed-stx))
              (equal? (syntax-e (attribute syntax-parse-id)) 'syntax-parse))
  
  (define-syntax-parse-rule (macro . pattern) new-body-part ...))


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
