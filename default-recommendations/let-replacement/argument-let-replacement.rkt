#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [argument-let-replacement refactoring-suite?]))


(require racket/list
         racket/set
         resyntax/base
         resyntax/default-recommendations/analyzers/function-expression-analyzer
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/pure-expression
         resyntax/default-recommendations/private/syntax-identifier-sets
         syntax/id-set
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


;; This rule extracts let expressions from function arguments into internal definitions.
;; It only applies when:
;; 1. We're in a function application (not a macro call)
;; 2. At least one argument is a let expression
;; 3. It's safe to extract (no identifier shadowing issues)


(define-syntax-class let-or-pure-expression
  #:attributes (body-expr [bind-id 1] [bind-rhs 1])
  #:literals (let)
  
  (pattern (let ([id:id rhs:expr] ...) inner:let-or-pure-expression)
    #:attr body-expr (attribute inner.body-expr)
    #:attr [bind-id 1] (append (attribute id) (attribute inner.bind-id))
    #:attr [bind-rhs 1] (append (attribute rhs) (attribute inner.bind-rhs)))
  
  (pattern body-expr:pure-expression
    #:attr [bind-id 1] '()
    #:attr [bind-rhs 1] '()))


(define-definition-context-refactoring-rule extract-lets-from-function-arguments
  #:description
  "Use internal definitions instead of `let` expressions inside function arguments to reduce nesting."
  #:analyzers (list function-expression-analyzer)
  
  (~seq leading-body ...
        (~and original-call
              (func:pure-expression arg:let-or-pure-expression ...+ arg-after ...)))
  
  #:when (equal? (syntax-property (attribute func) 'application-subexpression-kind) 'function)
  #:with (all-bind-id ...) (apply append (attribute arg.bind-id))
  #:with (all-bind-rhs ...) (apply append (attribute arg.bind-rhs))
  #:when (not (empty? (attribute all-bind-id)))
  
  (leading-body ...
   (~@ . (~focus-replacement-on
          (~splicing-replacement
           ((define all-bind-id all-bind-rhs) ... (func arg.body-expr ...))
           #:original original-call)))))


(define-refactoring-suite argument-let-replacement
  #:rules (extract-lets-from-function-arguments))
