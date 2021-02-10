#lang racket/base


(provide #%module-begin
         (rename-out [begin refactoring-test-program])
         quote
         refactoring-test-import
         refactoring-test-case)


(require syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-simple-macro (refactoring-test-import module rule)
  (begin 'module 'rule))


(define-simple-macro (refactoring-test-case name:str input:str expected:str)
  '(refactoring-test-case name input expected))


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
