#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [string-shortcuts refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule display-newline-to-newline
  #:description "The `newline` function can be used to print a single newline character."
  #:literals (display displayln)
  [(~or (display "\n") (displayln "")) (newline)])


(define string-shortcuts
  (refactoring-suite
   #:name (name string-shortcuts)
   #:rules
   (list display-newline-to-newline)))
