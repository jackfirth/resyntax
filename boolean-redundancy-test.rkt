#lang resyntax/test

(require racket/base)

;; Test cases for boolean redundancy patterns found in DrRacket's codebase
;; Location: DrRacket unit.rkt around line 2100 - canvas and frame object checking  
;; URL: https://github.com/racket/drracket/blob/master/drracket-core-lib/drracket/private/unit.rkt#L2100

(define-test-case "list-not-empty-check"
  ;; Common pattern: checking if list exists and is not empty
  #:original
  (and (list? items) (not (null? items)))
  
  #:expected
  (pair? items))

(define-test-case "list-not-empty-with-empty-predicate"
  ;; Alternative form using empty? instead of null?
  #:original
  (and (list? items) (not (empty? items)))
  
  #:expected
  (pair? items))

(define-test-case "boolean-and-object-redundancy"
  ;; Pattern where we check if object exists then use it in boolean context
  #:original
  (and text (is-a? text text%))
  
  #:expected
  (is-a? text text%)
  
  ;; This transformation is safe because is-a? returns #f for #f input
  #:with-free-variables ([text]))

(define-test-case "object-method-exists-pattern"
  ;; DrRacket pattern: check object exists and has capability
  #:original  
  (and editor (send editor get-admin))
  
  #:expected
  (and editor (send editor get-admin))
  
  ;; This should NOT be transformed because send has side effects
  ;; and we need the object check for safety
  #:should-not-transform? #t
  #:with-free-variables ([editor]))

(define-test-case "safe-boolean-object-check"
  ;; Only transform when the second expression is safe
  #:original
  (and filename (file-exists? filename))
  
  #:expected
  (and filename (file-exists? filename))
  
  ;; Should not transform - file-exists? could have side effects
  ;; and we want to preserve the explicit filename check
  #:should-not-transform? #t
  #:with-free-variables ([filename]))