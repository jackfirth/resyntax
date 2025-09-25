#lang resyntax/test


require: resyntax/default-recommendations deprecation-migrations


header:
------------------------------
#lang racket/base
(require resyntax/deprecated-alias-macro)
------------------------------


test: "use of deprecated alias refactorable to new name"
------------------------------
(define (a) 1)
(provide b)
(define-deprecated-alias b a)
(b)
------------------------------
------------------------------
(define (a) 1)
(provide b)
(define-deprecated-alias b a)
(a)
------------------------------
