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
   (define-syntax-parse-rule (ORIGINAL-GAP original first-form)
     (ORIGINAL-SPLICE first-form form ...))])


(define syntax-parse-shortcuts
  (refactoring-suite
   #:name (name syntax-parse-shortcuts)
   #:rules
   (list define-simple-macro-to-define-syntax-parse-rule)))
