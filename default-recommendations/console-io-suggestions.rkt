#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [console-io-suggestions refactoring-suite?]))


(require racket/file
         racket/list
         racket/string
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


(define-syntax-class printf-without-specifiers
  #:attributes (refactored)
  #:literals (printf)
  
  (pattern (printf "\n")
    #:with refactored #'(newline))
  
  (pattern (printf s:str)
    #:when (string-suffix? (syntax-e #'s) "\n")
    #:with stripped (substring (syntax-e #'s) 0 (- (string-length (syntax-e #'s)) 1))
    #:with refactored #'(displayln stripped))
  
  (pattern (printf s:str)
    #:with refactored #'(display s)))


(define-refactoring-rule printf-to-display
  #:description
  "This use of `printf` has no arguments other than the template string."
  expr:printf-without-specifiers
  expr.refactored)


(define-refactoring-suite console-io-suggestions
  #:rules (printf-to-display
           read-line-any))
