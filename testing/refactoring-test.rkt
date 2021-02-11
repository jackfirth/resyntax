#lang racket/base


(provide #%datum
         #%module-begin
         refactoring-test
         refactoring-test-import
         refactoring-test-case)


(require (for-syntax racket/base)
         racket/stxparam
         rackunit
         rebellion/collection/list
         rebellion/streaming/transducer
         resyntax
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(begin-for-syntax
  (define-syntax-class refactoring-test-import-statement
    #:attributes (require-statement rule)
    #:literals (refactoring-test-import)
    (pattern (refactoring-test-import module rule)
      #:with require-statement #'(require (only-in module rule)))))


(define-simple-macro (refactoring-test import:refactoring-test-import-statement ...+ case ...)
  (begin
    import.require-statement ...
    (define rule-list (refactoring-suite import.rule ...))
    (syntax-parameterize ([refactoring-rules-under-test (make-rename-transformer #'rule-list)])
      case ...)))


(define (refactoring-suite . rules-and-suites)
  (transduce rules-and-suites
             (append-mapping
              (λ (rule-or-suite) (if (list? rule-or-suite) rule-or-suite (list rule-or-suite))))
             #:into into-list))


(define-syntax (refactoring-test-import stx)
  (raise-syntax-error #false "can only be used within a refactoring test" stx))


(define-simple-macro (refactoring-test-case name:str input:str expected:str)
  (test-case name
    (define actual (refactor input #:rules refactoring-rules-under-test))
    (check-equal? actual expected)))


(define-syntax-parameter refactoring-rules-under-test
  (λ (stx) (raise-syntax-error #false "can only be used within a refactoring test case" stx)))


;@----------------------------------------------------------------------------------------------------


(module reader racket/base


  (require racket/contract/base)


  (provide
   (contract-out
    [read procedure?]
    [read-syntax procedure?]))


  (require resyntax/testing/refactoring-test-parser
           resyntax/testing/refactoring-test-tokenizer)
  

  ;@--------------------------------------------------------------------------------------------------


  (define (read in)
    (read-using-syntax-reader read-syntax in))


  (define (read-syntax source-name in)
    (define parse-tree (parse source-name (make-refactoring-test-tokenizer in)))
    (define module-datum
      `(module refactoring-test racket/base
         (module test resyntax/testing/refactoring-test
           ,parse-tree)))
    (datum->syntax #f module-datum))


  (define (read-using-syntax-reader syntax-reader in)
    (syntax->datum (syntax-reader #false in))))
