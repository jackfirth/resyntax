#lang racket/base


(provide #%app
         #%datum
         #%module-begin
         begin
         code-block
         header:
         require:
         statement
         test:)


(require (for-syntax racket/base
                     racket/sequence
                     resyntax/test/private/statement)
         racket/pretty
         racket/stxparam
         rackunit
         resyntax/base
         resyntax/test/private/rackunit
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-syntax (statement stx)
  (syntax-parse stx
    #:track-literals
    [(statement statement-id:id . tail)
     (define transformer (syntax-local-value #'statement-id (λ () #false)))
     (unless transformer
       (raise-syntax-error #false
                           "unbound identifier"
                           this-syntax
                           #'statement-id))
     (unless (statement-transformer? transformer)
       (raise-syntax-error #false
                           "not defined as a statement"
                           this-syntax
                           #'statement-id))
     (syntax-local-apply-transformer (statement-transformer-procedure transformer)
                                     #'statement-id
                                     'module
                                     #false
                                     stx)]))


(define-syntax require:
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ module:id suite:id)
        #`(begin
            (require (only-in module suite))
            ; Using syntax/loc to ensure that if add-suite-under-test! throws a runtime
            ; error because suite isn't a refactoring suite, the error will point to the
            ; require: statement.
            #,(syntax/loc this-syntax (add-suite-under-test! suite)))]))))


(begin-for-syntax
  (define-syntax-class literal-code-block
    #:description "a code block"
    #:opaque
    #:literals (code-block)
    (pattern (code-block str:str))))


(define-syntax header:
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ header-code:literal-code-block)
        ; Using syntax/loc so that errors thrown by set-header! point to the header:
        ; statement.
        (syntax/loc stx (set-header! header-code))]))))


(define-syntax test:
  (statement-transformer
   (λ (stx)
     (syntax-parse stx
       #:track-literals
       [(_ _ name:str code:literal-code-block)
        #`(test-case name
            #,(syntax/loc stx
                (check-suite-does-not-refactor code)))]
       [(_ _ name:str input-code:literal-code-block expected-code:literal-code-block)
        #`(test-case name
            #,(syntax/loc stx
                (check-suite-refactors input-code expected-code)))]
       [(_ _ name:str
           input-code:literal-code-block ...+
           expected-code:literal-code-block)
        #:when (>= (length (attribute input-code)) 2)
        #`(test-case name
            #,@(for/list ([input-stx (in-list (attribute input-code))])
                 (quasisyntax/loc input-stx
                   (check-suite-refactors #,input-stx expected-code))))]))))


(define-syntax (refactoring-test-case stx)
  (define (add-header input-stx)
    #`(string-append implicit-program-header #,input-stx))
  (syntax-parse stx
    [(_ name:str input:str)
     #:cut
     #`(test-case name
         #,(quasisyntax/loc this-syntax
             (check-suite-does-not-refactor refactoring-suite-under-test #,(add-header #'input))))]
    [(_ name:str input:str expected:str)
     #:cut
     #`(test-case name
         #,(quasisyntax/loc this-syntax
             (check-suite-refactors
              refactoring-suite-under-test #,(add-header #'input) #,(add-header #'expected))))]
    [(_ name:str input:str ...+ expected:str)
     #:cut
     #:with expected-with-header (add-header #'expected)
     #`(test-case name
         #,@(for/list ([input-stx (in-syntax #'(input ...))])
              (quasisyntax/loc input-stx
                (check-suite-refactors
                 refactoring-suite-under-test #,(add-header input-stx) expected-with-header))))]))


(define-syntax-parameter refactoring-suite-under-test
  (λ (stx) (raise-syntax-error #false "can only be used within a refactoring test case" stx)))


(define-syntax-parameter implicit-program-header
  (syntax-parser #:literals (implicit-program-header) [implicit-program-header #'""]))


;@----------------------------------------------------------------------------------------------------


(module reader racket/base


  (require racket/contract/base)


  (provide
   (contract-out
    [read procedure?]
    [read-syntax procedure?]))


  (require resyntax/test/private/grammar
           resyntax/test/private/tokenizer)
  

  ;@--------------------------------------------------------------------------------------------------


  (define (read in)
    (read-using-syntax-reader read-syntax in))


  (define (read-syntax source-name in)
    (define parse-tree (parse source-name (make-refactoring-test-tokenizer in)))
    (define module-datum
      `(module refactoring-test racket/base
         (module test resyntax/test
           ,parse-tree)))
    (datum->syntax #f module-datum))


  (define (read-using-syntax-reader syntax-reader in)
    (syntax->datum (syntax-reader #false in))))
