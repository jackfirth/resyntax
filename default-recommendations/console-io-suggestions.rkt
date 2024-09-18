#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [console-io-suggestions refactoring-suite?]))


(require racket/file
         racket/list
         rebellion/private/static-name
         resyntax/base
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule read-line-any
  #:description
  (string-append "Specify a line mode of `'any` with `read-line` to avoid differences between "
                 "Windows and other platforms.")
  #:literals (read-line)
  (read-line (~optional port))
  (read-line (~? port (current-input-port)) 'any))


(define-refactoring-suite console-io-suggestions
  #:rules (read-line-any))
