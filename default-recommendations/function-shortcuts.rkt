#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [function-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/literal-constant
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class unquoted
  #:attributes (expr)
  #:literals (unquote)
  (pattern expr:literal-constant)
  (pattern (unquote expr:expr)))


(define-syntax-class trailing-list-argument
  #:attributes ([lifted 1] trailing)
  #:literals (cons list* quasiquote unquote-splicing)

  (pattern (cons arg rest:trailing-list-argument)
    #:cut
    #:with (lifted ...) #'(arg rest.lifted ...)
    #:with trailing #'rest.trailing)

  (pattern (cons arg trailing:expr)
    #:cut
    #:with (lifted ...) #'(arg))

  (pattern (list* arg ... rest:trailing-list-argument)
    #:cut
    #:with (lifted ...) #'(arg ... rest.lifted ...)
    #:with trailing #'rest.trailing)

  (pattern (list* arg ... rest:expr)
    #:cut
    #:with (lifted ...) #'(arg ...)
    #:with trailing #'rest)

  (pattern (quasiquote (arg:unquoted ... (unquote-splicing rest)))
    #:cut
    #:with (lifted ...) #'(arg.expr ...)
    #:with trailing #'rest))


(define-refactoring-rule apply-flattening
  #:description
  "The `apply` function accepts single arguments in addition to a trailing list argument."
  #:literals (apply)
  ((~and id apply) function:expr arg ... trailing-arg:trailing-list-argument)
  (id function arg ... trailing-arg.lifted ... trailing-arg.trailing))


(define-refactoring-rule case-lambda-with-single-case-to-lambda
  #:description "This `case-lambda` form only has one case. Use a regular lambda instead."
  #:literals (case-lambda)
  (case-lambda [args body ...])
  (λ args body ...))


(define function-shortcuts
  (refactoring-suite
   #:name (name function-shortcuts)
   #:rules
   (list apply-flattening
         case-lambda-with-single-case-to-lambda)))
