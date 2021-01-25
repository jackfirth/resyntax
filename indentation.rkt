#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [indent-code (-> immutable-string? natural? natural? immutable-string?)]))


(require framework
         racket/class
         racket/math
         rebellion/base/immutable-string)


(module+ main
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (indent-code code-string start end)
  (define text-object (new racket:text%))
  (send text-object insert code-string)
  (send text-object set-position start end)
  (send text-object tabify-selection)
  (string->immutable-string (send text-object get-text)))


;; Empty test submodule to prevent initialization of the GUI framework in CI.
(module test racket/base)


;; This test can't be run from CI because of GUI dependencies, so we put it in a main submodule and
;; only run it manually.
(module+ main
  (test-case "indent-code"
    (check-equal?
     (indent-code "#lang racket/base\n\n(+ 1\n2\n3)\n\n(+ 4\n5\n6)\n" 19 28)
     "#lang racket/base\n\n(+ 1\n   2\n   3)\n\n(+ 4\n5\n6)\n")
    (check-equal?
     (indent-code "#lang racket/base\n\n(+ 1\n2\n3)\n\n(+ 4\n5\n6)\n" 30 39)
     "#lang racket/base\n\n(+ 1\n2\n3)\n\n(+ 4\n   5\n   6)\n")))
