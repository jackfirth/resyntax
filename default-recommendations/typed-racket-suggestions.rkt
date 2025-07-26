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


(define-refactoring-rule tr-constructor-abbreviation-pattern
  #:description "This constructor abbreviation follows the Typed Racket convention of prefixing constructors with '-'."
  #:literals (define)
  (define abbrev-name:id make-constructor:id)
  #:when (let ([abbrev-str (symbol->string (syntax-e #'abbrev-name))]
               [make-str (symbol->string (syntax-e #'make-constructor))])
           (and (string-prefix? abbrev-str "-")
                (string-prefix? make-str "make-")
                ;; The name after '-' should match the name after 'make-' (case insensitive)
                (string-ci=? (substring abbrev-str 1)
                             (substring make-str 5))))
  #:no-replacement)


(define-refactoring-rule tr-parametric-constructor-pattern
  #:description "This parametric constructor follows a common Typed Racket pattern for constructors with optional arguments."
  #:literals (define)
  (define (constructor-name:id param1:id [param2:id default-val:expr]) (make-constructor:id arg1:id arg2:id))
  #:when (let ([constructor-str (symbol->string (syntax-e #'constructor-name))]
               [make-str (symbol->string (syntax-e #'make-constructor))])
           (and (string-prefix? constructor-str "-")
                (string-prefix? make-str "make-")
                (string-ci=? (substring constructor-str 1)
                             (substring make-str 5))))
  #:no-replacement)


(define-refactoring-suite typed-racket-suggestions
  #:rules (tr-constructor-abbreviation-pattern
           tr-parametric-constructor-pattern))