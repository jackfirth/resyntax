#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-parse-shortcuts refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule define-simple-macro-to-define-syntax-parse-rule
  #:description "The define-simple-macro form has been renamed to define-syntax-parse-rule."
  #:literals (define-simple-macro)
  [((~and original define-simple-macro) first-form form ...)

   ;; The define-simple-macro is a renamed alias of define-syntax-parse-rule, so it's
   ;; free-identifier=?. As a result, we need to check the actual symbol of the identifier instead of
   ;; just its binding. See https://github.com/jackfirth/resyntax/issues/106.
   #:when (equal? (syntax-e #'original) 'define-simple-macro)
   
   (define-syntax-parse-rule (ORIGINAL-GAP original first-form)
     (ORIGINAL-SPLICE first-form form ...))])


(define syntax-parse-shortcuts
  (refactoring-suite
   #:name (name syntax-parse-shortcuts)
   #:rules
   (list define-simple-macro-to-define-syntax-parse-rule)))
