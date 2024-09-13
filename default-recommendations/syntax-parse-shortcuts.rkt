#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-parse-shortcuts refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule define-simple-macro-to-define-syntax-parse-rule
  #:description "The `define-simple-macro` form has been renamed to `define-syntax-parse-rule`."
  #:literals (define-simple-macro)
  (original:define-simple-macro header form ...)

  ;; The define-simple-macro is a renamed alias of define-syntax-parse-rule, so it's
  ;; free-identifier=?. As a result, we need to check the actual symbol of the identifier instead of
  ;; just its binding. See https://github.com/jackfirth/resyntax/issues/106.
  #:when (equal? (syntax-e #'original) 'define-simple-macro)
   
  ((~replacement define-syntax-parse-rule #:original original) header form ...))


(define-refactoring-suite syntax-parse-shortcuts
  #:rules (define-simple-macro-to-define-syntax-parse-rule))
