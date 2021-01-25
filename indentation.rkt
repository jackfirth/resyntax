#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [indent-code (-> immutable-string? natural? natural? immutable-string?)]))


(require framework
         racket/class
         racket/math
         rebellion/base/immutable-string)


;@----------------------------------------------------------------------------------------------------


(define (indent-code code-string start end)
  (define text-object (new racket:text%))
  (send text-object insert code-string)
  (send text-object set-position start end)
  (send text-object tabify-selection)
  (string->immutable-string (send text-object get-text)))


;; Empty test submodule to prevent initialization of the GUI framework in CI.
(module test racket/base)
