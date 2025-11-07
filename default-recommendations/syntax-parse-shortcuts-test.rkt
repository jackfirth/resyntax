#lang resyntax/test


require: resyntax/default-recommendations syntax-parse-shortcuts


header:
------------------------------
#lang racket/base
(require (for-syntax racket/base syntax/parse)
         syntax/parse/define)
------------------------------


test: "define-syntax with syntax-parse and one clause refactorable to define-syntax-parse-rule"
------------------------------
(define-syntax my-or
  (lambda (stx)
    (syntax-parse stx
      [(_ a b)
       #'(let ([tmp a]) (if tmp tmp b))])))
==============================
(define-syntax-parse-rule (my-or a b)
  #'(let ([tmp a]) (if tmp tmp b)))
------------------------------


test: "define-syntax-parser with one clause refactorable to define-syntax-parse-rule"
------------------------------
(define-syntax-parser my-or
  [(_ a b)
   #'(let ([tmp a]) (if tmp tmp b))])
==============================
(define-syntax-parse-rule (my-or a b)
  #'(let ([tmp a]) (if tmp tmp b)))
------------------------------


test: "define-syntax with syntax-parse using stx name refactorable to define-syntax-parse-rule"
------------------------------
(define-syntax my-macro
  (lambda (stx)
    (syntax-parse stx
      [(_ x:id)
       #'(quote x)])))
==============================
(define-syntax-parse-rule (my-macro x:id)
  #'(quote x))
------------------------------


no-change-test: "define-syntax with syntax-parse and multiple clauses not refactorable"
------------------------------
(define-syntax my-or
  (lambda (stx)
    (syntax-parse stx
      [(_ a b)
       #'(let ([tmp a]) (if tmp tmp b))]
      [(_ a)
       #'a])))
------------------------------


no-change-test: "define-syntax-parser with multiple clauses not refactorable"
------------------------------
(define-syntax-parser my-or
  [(_ a b)
   #'(let ([tmp a]) (if tmp tmp b))]
  [(_ a)
   #'a])
------------------------------


no-change-test: "define-syntax without syntax-parse not refactorable"
------------------------------
(define-syntax my-or
  (syntax-rules ()
    [(_ a b)
     (let ([tmp a]) (if tmp tmp b))]))
------------------------------
