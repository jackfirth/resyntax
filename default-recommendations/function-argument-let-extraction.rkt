#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [function-argument-let-extraction refactoring-suite?]))


(require racket/list
         racket/set
         resyntax/base
         resyntax/default-recommendations/analyzers/function-expression-analyzer
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/syntax-identifier-sets
         syntax/id-set
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


;; This rule extracts let expressions from function arguments into internal definitions.
;; It only applies when:
;; 1. We're in a function application (not a macro call)
;; 2. At least one argument is a let expression
;; 3. It's safe to extract (no identifier shadowing issues)


(define-syntax-class function-arg
  #:attributes (body-expr [bind-id 1] [bind-rhs 1])
  #:literals (let)
  
  ;; Case 1: argument is a let expression  
  (pattern (~and let-arg (let ([id:id rhs:expr] ...) inner:function-arg))
    #:attr body-expr (attribute inner.body-expr)
    #:attr [bind-id 1] (append (attribute id) (attribute inner.bind-id))
    #:attr [bind-rhs 1] (append (attribute rhs) (attribute inner.bind-rhs)))
  
  ;; Case 2: argument is not a let expression
  (pattern other:expr
    #:attr body-expr #'other
    #:attr [bind-id 1] '()
    #:attr [bind-rhs 1] '()))


(define-definition-context-refactoring-rule extract-lets-from-function-arguments
  #:description
  "Internal definitions are recommended instead of `let` expressions nested in function arguments."
  #:analyzers (list function-expression-analyzer)
  
  (~seq leading-body ...
        (~and original-call
              (func:expr arg:function-arg ...+)))
  
  ;; Only apply if func is actually used as a function (not a macro)
  #:when (equal? (syntax-property (attribute func) 'application-subexpression-kind) 'function)
  
  ;; Collect all bindings from all arguments
  #:with (all-bind-id ...) (apply append (attribute arg.bind-id))
  #:with (all-bind-rhs ...) (apply append (attribute arg.bind-rhs))
  
  ;; Only apply if at least one argument has a let expression
  #:when (not (null? (attribute all-bind-id)))
  
  ;; Check that none of the new bindings have duplicate names
  #:when (let ([names (map syntax-e (attribute all-bind-id))])
           (= (length names) (length (remove-duplicates names))))
  
  (leading-body ...
   (~@ . (~focus-replacement-on
          (~splicing-replacement
           ((define all-bind-id all-bind-rhs) ... (func arg.body-expr ...))
           #:original original-call)))))


(define-refactoring-suite function-argument-let-extraction
  #:rules (extract-lets-from-function-arguments))
