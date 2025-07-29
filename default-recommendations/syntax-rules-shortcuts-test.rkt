#lang resyntax/test

require:
resyntax/default-recommendations
syntax-rules-shortcuts

header:
-
#lang racket/base

test:
"single-clause syntax-rules macro refactorable to define-syntax-rule"
------------------------------
(define-syntax my-or
  (syntax-rules ()
    [(my-or a b) (let ([tmp a]) (if a a b))]))
==============================
(define-syntax-rule (my-or a b)
  (let ([tmp a]) (if a a b)))
------------------------------

test:
"single-clause syntax-rules macro not referring to name refactorable to define-syntax-rule"
------------------------------
(define-syntax my-or
  (syntax-rules ()
    [(_ a b) (let ([tmp a]) (if a a b))]))
==============================
(define-syntax-rule (my-or a b)
  (let ([tmp a]) (if a a b)))
------------------------------
