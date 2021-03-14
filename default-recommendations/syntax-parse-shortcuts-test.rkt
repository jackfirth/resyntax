#lang resyntax/testing/refactoring-test


require: resyntax/default-recommendations syntax-parse-shortcuts


header:
------------------------------
#lang racket/base
(require syntax/parse/define)
------------------------------


test: "define-simple-macro refactorable to define-syntax-parse-rule"
------------------------------
(define-simple-macro (my-or a:expr b:expr)
  (let ([tmp a])
    (if a a b)))
------------------------------
------------------------------
(define-syntax-parse-rule (my-or a:expr b:expr)
  (let ([tmp a])
    (if a a b)))
------------------------------


test: "define-simple-macro with header on next line refactorable to define-syntax-parse-rule"
------------------------------
(define-simple-macro
  (my-or a:expr b:expr)
  (let ([tmp a])
    (if a a b)))
------------------------------
------------------------------
(define-syntax-parse-rule
  (my-or a:expr b:expr)
  (let ([tmp a])
    (if a a b)))
------------------------------


test: "define-simple-macro with body comments refactorable to define-syntax-parse-rule"
------------------------------
(define-simple-macro (my-or a:expr b:expr)
  ;; The let form is needed to avoid evaluating a twice.
  (let ([tmp a])
    (if a a b)))
------------------------------
------------------------------
(define-syntax-parse-rule (my-or a:expr b:expr)
  ;; The let form is needed to avoid evaluating a twice.
  (let ([tmp a])
    (if a a b)))
------------------------------
