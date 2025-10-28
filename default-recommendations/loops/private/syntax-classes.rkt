#lang racket/base


(provide worthwhile-loop-body-function)


(require resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/let-replacement/private/let-binding
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


;; A loop body function is a lambda expression that is passed to a function like map, for-each, or
;; ormap which calls the lambda once for each element of a list. When code is migrated to use for
;; loops, the loop body function becomes the body of the for loop, hence the name. For convenience,
;; we also accept lambdas which take two arguments such as those used with hash-for-each. Techncially,
;; such a two-argument lambda shouldn't be accepted when in the context of a function like for-each
;; instead of hash-for-each, but we don't bother checking for that since if the code already compiles
;; and runs without any tests failing it probably doesn't have that issue.
(define-syntax-class worthwhile-loop-body-function
  #:attributes (x y [body 1])

  ;; We always migrate loop functions that use let expressions, since in the process of migrating
  ;; we can replace the let bindings with internal definitions within the for loop body.
  (pattern
    (_:lambda-by-any-name (x (~optional (~seq y)))
                          original-body:body-with-refactorable-let-expression)
    #:with (body ...) #'(original-body.refactored ...))

  ;; Lambdas with multiple body forms are hard to read when all the forms are on one line, so we
  ;; assume all such lambdas are multi-line, and multi-line for-each functions are typically easier
  ;; to read when they're in the body of a for loop.
  (pattern (_:lambda-by-any-name (x (~optional (~seq y))) first-body remaining-body ...+)
    #:with (body ...) #'(first-body remaining-body ...))

  ;; We don't bother migrating for-each forms with only a single body form unless the body form is
  ;; exceptionally long, so that forms which span multiple lines tend to get migrated. By not
  ;; migrating short forms, we avoid bothering reviewers with changes to loops that aren't complex
  ;; enough to need a lot of refactoring in the first place.
  (pattern (_:lambda-by-any-name (x (~optional (~seq y))) only-body)
    #:when (>= (syntax-span #'only-body) 60)
    #:with (body ...) #'(only-body)))


(module+ test
  (test-case "worthwhile-loop-body-function"

    (define (worthwhile-loop-body-function? stx)
      (syntax-parse stx [:worthwhile-loop-body-function #true] [_  #false]))

    (define (x-attribute stx)
      (syntax-parse stx [:worthwhile-loop-body-function (attribute x)]))

    (define (y-attribute stx)
      (syntax-parse stx [:worthwhile-loop-body-function (attribute y)]))

    (define (body-attribute stx)
      (syntax-parse stx [:worthwhile-loop-body-function (attribute body)]))

    (test-case "loop function with let binding"
      (define stx #'(λ (arg) (let ([foo (+ arg 1)]) (* foo foo))))
      (check-true (worthwhile-loop-body-function? stx))
      (check-equal? (syntax->datum (x-attribute stx)) 'arg)
      (check-false (y-attribute stx))
      (check-equal? (map syntax->datum (body-attribute stx))
                    (list '(define foo (+ arg 1)) '(* foo foo))))))
