#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [deprecation-migrations refactoring-suite?]))


(require racket/class
         resyntax/base
         resyntax/deprecated-alias
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define (resyntax-phase1-eval compile-time-expression-stx #:quote-syntax? [quote-syntax? #false])
  (namespace-require 'syntax/macro-testing)
  (namespace-require '(for-syntax racket/base))
  (if quote-syntax?
      (eval `(phase1-eval ,(syntax->datum compile-time-expression-stx) #:quote quote-syntax))
      (eval `(phase1-eval ,(syntax->datum compile-time-expression-stx)))))


(define-refactoring-rule inline-deprecated-alias
  #:description "This form has been renamed. Use the new name instead of the deprecated alias."

  (alias-id:id . tail)

  #:do [(namespace-require '(for-syntax resyntax/deprecated-alias))]
  #:when (resyntax-phase1-eval
          #'(let-values ([(immediate _)
                          (syntax-local-value/immediate #'alias-id (λ () (values #false #false)))])
              (deprecated-alias? immediate)))
  #:do [(define target-id-symbol
          (resyntax-phase1-eval
           #'(let-values ([(immediate _)
                           (syntax-local-value/immediate #'alias-id
                                                         (λ () (values #false #false)))])
               (deprecated-alias-target immediate))))]
  #:with target-id (datum->syntax #'alias-id target-id-symbol)

  (target-id . tail))


(define-refactoring-suite deprecation-migrations
  #:rules (inline-deprecated-alias))
