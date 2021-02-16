#lang racket/base


(provide #%datum
         #%module-begin
         refactoring-test
         refactoring-test-import
         refactoring-test-case)


(require (for-syntax racket/base)
         racket/pretty
         racket/stxparam
         rackunit
         rackunit/private/check-info
         rebellion/collection/list
         rebellion/streaming/transducer
         resyntax
         resyntax/refactoring-suite
         resyntax/string-replacement
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(begin-for-syntax
  (define-syntax-class refactoring-test-import-statement
    #:attributes (require-statement suite)
    #:literals (refactoring-test-import)
    (pattern (refactoring-test-import module suite)
      #:with require-statement #'(require (only-in module suite)))))


(define-simple-macro (refactoring-test import:refactoring-test-import-statement ... case ...)
  (begin
    import.require-statement ...
    (define suite
      (refactoring-suite #:rules (append (refactoring-suite-rules import.suite) ...)))
    (syntax-parameterize ([refactoring-suite-under-test (make-rename-transformer #'suite)])
      case ...
      ;; this void expression ensures that it's not an error if no test cases are given
      (void))))


(define-syntax (refactoring-test-import stx)
  (raise-syntax-error #false "can only be used within a refactoring test" stx))


(define-simple-macro (refactoring-test-case name:str input:str (~optional expected:str))
  #:with check
  (syntax/loc this-syntax (check-equal? (string-block actual) (string-block (~? expected actual))))
  (test-case name
    (define replacement (refactor input #:suite refactoring-suite-under-test))
    (with-check-info (['replacement (pretty-info replacement)])
      (define actual (string-apply-replacement input replacement))
      check)))


(define-syntax-parameter refactoring-suite-under-test
  (λ (stx) (raise-syntax-error #false "can only be used within a refactoring test case" stx)))


(struct string-block (raw-string) #:transparent
  #:guard (λ (raw-string _) (string->immutable-string raw-string))

  #:methods gen:custom-write

  [(define (write-proc this out mode)
     (define raw (string-block-raw-string this))
     (define-values (_line col _pos) (port-next-location out))
     (cond
       [(and (pretty-printing) (integer? (pretty-print-columns)) col)
        (define lead (make-string col #\space))
        (for ([line (in-lines (open-input-string raw))]
              [i (in-naturals)])
          (unless (zero? i)
            (write-string lead out)
            (pretty-print-newline out (pretty-print-columns)))
          (write-string line out))]
       [else
        (for ([line (in-lines (open-input-string raw))]
              [i (in-naturals)])
          (unless (zero? i)
            (newline out))
          (write-string line out))]))])


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
