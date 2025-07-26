#lang resyntax/test

(require racket/base)

;; Test cases for conditional assignment patterns found in DrRacket
;; Location: DrRacket unit.rkt around line 4200 - button state management
;; URL: https://github.com/racket/drracket/blob/master/drracket-core-lib/drracket/private/unit.rkt#L4200

(define-test-case "cond-with-void-else-to-when"
  ;; Pattern: cond with single condition and void else clause
  #:original
  (cond
    [(send button is-enabled?)
     (send button enable #f)]
    [else (void)])
  
  #:expected
  (when (send button is-enabled?)
    (send button enable #f)))

(define-test-case "cond-no-else-to-when" 
  ;; Pattern: cond with single condition and no else (implicit void)
  #:original
  (cond
    [(and frame (is-a? frame drracket:unit:frame<%>))
     (send frame update-save-button)])
  
  #:expected
  (when (and frame (is-a? frame drracket:unit:frame<%>))
    (send frame update-save-button)))

(define-test-case "cond-multiple-actions-to-when"
  ;; Pattern: cond with multiple actions in then clause
  #:original
  (cond
    [definitions-shown?
     (toggle-show/hide-definitions)
     (update-shown)]
    [else (void)])
  
  #:expected
  (when definitions-shown?
    (toggle-show/hide-definitions)
    (update-shown)))

(define-test-case "cond-with-multiple-clauses-no-transform"
  ;; Should NOT transform: multiple non-else clauses
  #:original
  (cond
    [(eq? state 'running) (stop-execution)]
    [(eq? state 'stopped) (start-execution)]
    [else (void)])
  
  #:expected
  (cond
    [(eq? state 'running) (stop-execution)]
    [(eq? state 'stopped) (start-execution)]
    [else (void)])
  
  #:should-not-transform? #t)

(define-test-case "cond-with-meaningful-else-no-transform"
  ;; Should NOT transform: else clause does something meaningful
  #:original
  (cond
    [condition (action1)]
    [else (action2)])
  
  #:expected  
  (cond
    [condition (action1)]
    [else (action2)])
  
  #:should-not-transform? #t)