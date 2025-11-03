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


(define-refactoring-rule printf-newline-only
  #:description "The `newline` function can be used to print a single newline character."
  #:literals (printf)
  (printf "\n")
  (newline))


(define-refactoring-rule printf-ending-with-newline
  #:description
  "When `printf` is used with a single string argument ending in a newline, use `displayln`."
  #:literals (printf)
  (printf s:str)
  #:when (string-suffix? (syntax-e #'s) "\n")
  #:when (not (equal? (syntax-e #'s) "\n"))
  #:with stripped (substring (syntax-e #'s) 0 (- (string-length (syntax-e #'s)) 1))
  (displayln stripped))


(define-refactoring-rule printf-without-newline
  #:description "When `printf` is used with a single string argument, use `display`."
  #:literals (printf)
  (printf s:str)
  #:when (not (string-suffix? (syntax-e #'s) "\n"))
  (display s))


;; Rules are ordered from most specific to least specific. printf-newline-only handles the
;; special case of "\n", printf-ending-with-newline handles strings ending with newlines,
;; and printf-without-newline handles all other strings.
(define-refactoring-suite console-io-suggestions
  #:rules (printf-newline-only
           printf-ending-with-newline
           printf-without-newline
           read-line-any))
