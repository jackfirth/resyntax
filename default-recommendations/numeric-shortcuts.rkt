#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [numeric-shortcuts refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule add1-lambda-to-add1
  #:description "This lambda function is equivalent to the built-in add1 function."
  #:literals (+)
  [(lambda:lambda-by-any-name (x1:id) (~or (+ x2:id 1) (+ 1 x2:id)))
   #:when (free-identifier=? #'x1 #'x2)
   add1])


(define-refactoring-rule sub1-lambda-to-sub1
  #:description "This lambda function is equivalent to the built-in sub1 function."
  #:literals (+ -)
  [(lambda:lambda-by-any-name (x1:id) (~or (- x2:id 1) (+ x2:id -1) (+ -1 x2:id)))
   #:when (free-identifier=? #'x1 #'x2)
   sub1])


(define-refactoring-rule zero-comparison-lambda-to-positive?
  #:description "This lambda function is equivalent to the built-in positive? predicate."
  #:literals (< >)
  [(lambda:lambda-by-any-name (x1:id) (~or (> x2:id 0) (< 0 x2:id)))
   #:when (free-identifier=? #'x1 #'x2)
   positive?])


(define-refactoring-rule zero-comparison-lambda-to-negative?
  #:description "This lambda function is equivalent to the built-in negative? predicate."
  #:literals (< >)
  [(lambda:lambda-by-any-name (x1:id) (~or (< x2:id 0) (> 0 x2:id)))
   #:when (free-identifier=? #'x1 #'x2)
   negative?])


(define-refactoring-rule single-argument-plus-to-identity
  #:description "This expression is equivalent to the identity."
  #:literals (+)
  [(+ e) e])


(define-refactoring-rule single-argument-multiply-to-identity
  #:description "This expression is equivalent to the identity."
  #:literals (*)
  [(* e) e])


(define numeric-shortcuts
  (refactoring-suite
   #:name (name numeric-shortcuts)
   #:rules
   (list add1-lambda-to-add1
         single-argument-multiply-to-identity
         single-argument-plus-to-identity
         sub1-lambda-to-sub1
         zero-comparison-lambda-to-negative?
         zero-comparison-lambda-to-positive?)))
