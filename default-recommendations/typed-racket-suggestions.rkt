#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [typed-racket-suggestions refactoring-suite?]))


(require rebellion/private/static-name
         resyntax/base
         syntax/parse
         racket/string)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule tr-dash-app-constructor-pattern
  #:description "This constructor abbreviation follows a common Typed Racket pattern."
  #:literals (define)
  (define -App make-App)
  #:no-replacement)


(define-refactoring-rule tr-dash-box-constructor-pattern
  #:description "This constructor abbreviation follows a common Typed Racket pattern."
  #:literals (define)
  (define -box make-Box)
  #:no-replacement)


(define-refactoring-rule tr-dash-channel-constructor-pattern
  #:description "This constructor abbreviation follows a common Typed Racket pattern."
  #:literals (define)
  (define -channel make-Channel)
  #:no-replacement)


(define-refactoring-rule tr-lambda-wrapper-simplification
  #:description "This lambda wrapper around a constructor can be simplified to a direct reference."
  #:literals (define lambda)
  (define wrapper:id (lambda args:id make-name:id))
  #:when (and (identifier? #'args)
              (identifier? #'make-name)
              (let ([make-str (symbol->string (syntax-e #'make-name))])
                (string-prefix? make-str "make-")))
  (define wrapper make-name))


(define-refactoring-suite typed-racket-suggestions
  #:rules (tr-dash-app-constructor-pattern
           tr-dash-box-constructor-pattern
           tr-dash-channel-constructor-pattern
           tr-lambda-wrapper-simplification))