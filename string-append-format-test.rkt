#lang resyntax/test

(require racket/base racket/format)

;; Test cases for string concatenation patterns found in DrRacket
;; Location: DrRacket unit.rkt around line 2800 - building menu labels
;; URL: https://github.com/racket/drracket/blob/master/drracket-core-lib/drracket/private/unit.rkt#L2800

(define-test-case "format-single-value-to-~a"
  ;; Simple format ~a conversion
  #:original
  (format "~a" value)
  
  #:expected
  (~a value))

(define-test-case "string-append-with-format-to-~a"
  ;; String concatenation with format in the middle
  #:original
  (string-append "Error in " (format "~a" filename) ": ")
  
  #:expected
  (~a "Error in " filename ": "))

(define-test-case "multiple-format-in-string-append"
  ;; Multiple format calls in string-append
  #:original
  (string-append (format "~a" name) " version " (format "~a" version))
  
  #:expected
  (~a name " version " version))

(define-test-case "string-append-numbers-and-strings"
  ;; Mixed numbers and strings - common in DrRacket for status messages
  #:original
  (string-append "Line " (format "~a" line-num) ", Column " (format "~a" col-num))
  
  #:expected
  (~a "Line " line-num ", Column " col-num))

(define-test-case "format-with-complex-string-no-transform"
  ;; Should NOT transform: format with complex format string
  #:original
  (format "~a: ~s" key value)
  
  #:expected
  (format "~a: ~s" key value)
  
  #:should-not-transform? #t)

(define-test-case "string-append-no-format-no-transform"
  ;; Should NOT transform: no format calls
  #:original
  (string-append "prefix" middle "suffix")
  
  #:expected
  (string-append "prefix" middle "suffix")
  
  #:should-not-transform? #t)

(define-test-case "drracket-memory-limit-example"
  ;; Real example from DrRacket memory limit display
  #:original
  (string-append "Memory limit: " (format "~a" (floor (/ limit 1024 1024))) " MB")
  
  #:expected
  (~a "Memory limit: " (floor (/ limit 1024 1024)) " MB"))