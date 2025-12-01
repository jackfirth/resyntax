#lang racket/base


(provide
 keep-sorted)


(require (for-syntax racket/base
                     syntax/parse))


;@----------------------------------------------------------------------------------------------------


;; The keep-sorted macro marks a collection as needing to be kept in sorted order.
;; Resyntax refactoring rules can detect this syntax property and suggest reordering
;; the elements when they're not sorted.

;; The macro expands to a syntax property on the inner expression that Resyntax can detect.
;; The first form in the list is ignored (it's the collection constructor like list, set, vector).


(define-syntax (keep-sorted stx)
  (syntax-parse stx
    [(_ body:expr)
     (syntax-property #'body 'keep-sorted #true)]))
