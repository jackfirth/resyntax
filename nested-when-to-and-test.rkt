#lang resyntax/test

(require racket/base)

;; Test case for nested when expressions found in DrRacket's unit.rkt
;; Location: DrRacket unit.rkt around line 3890 - frame and canvas checking
;; URL: https://github.com/racket/drracket/blob/master/drracket-core-lib/drracket/private/unit.rkt#L3890

(define-test-case "nested-when-expressions-basic"
  ;; Basic nested when pattern
  #:original
  (when frame
    (when (is-a? frame drracket:unit:frame<%>)
      (send frame method)))
  
  #:expected  
  (when (and frame (is-a? frame drracket:unit:frame<%>))
    (send frame method)))

(define-test-case "nested-when-with-multiple-actions"
  ;; Nested when with multiple actions in inner when
  #:original
  (when user-custodian
    (when (thread-running? user-thread)
      (custodian-shutdown-all user-custodian)
      (set! user-custodian #f)))
  
  #:expected
  (when (and user-custodian (thread-running? user-thread))
    (custodian-shutdown-all user-custodian)
    (set! user-custodian #f)))

(define-test-case "nested-when-three-levels"
  ;; Triple nested when (should only collapse outermost two)
  #:original
  (when a
    (when b
      (when c
        action)))
  
  #:expected
  (when (and a b)
    (when c
      action)))

(define-test-case "nested-when-with-complex-conditions"
  ;; More complex conditions from DrRacket rep.rkt
  #:original
  (when (get-user-thread)
    (when (thread-running? (get-user-thread))
      (cleanup-interaction)))
  
  #:expected
  (when (and (get-user-thread) (thread-running? (get-user-thread)))
    (cleanup-interaction)))