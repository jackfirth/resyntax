#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [exception-suggestions refactoring-suite?]))


(require resyntax/base
         resyntax/default-recommendations/private/literal-constant
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule with-handlers-literal-handler-to-lambda
  #:description
  "A `with-handlers` handler should be a procedure. Wrap this literal in a lambda."
  #:literals (with-handlers)
  
  (with-handlers (clause-before ...
                  [pred:expr handler:literal-constant]
                  clause-after ...)
    body:expr ...)
  
  (with-handlers (clause-before ...
                  [pred (lambda (_) handler)]
                  clause-after ...)
    body ...))


(define-refactoring-suite exception-suggestions
  #:rules (with-handlers-literal-handler-to-lambda))
