#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [refactoring-rule? predicate/c]
  [let-to-block refactoring-rule?]
  [single-block-elimination refactoring-rule?]
  [immediate-define-block-elimination refactoring-rule?]))

(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor (-> refactoring-rule? syntax? (option/c syntax?))])))

(require (for-template racket/block)
         fancy-app
         racket/block
         racket/format
         racket/function
         racket/match
         racket/port
         racket/pretty
         racket/sequence
         racket/set
         racket/string
         racket/syntax
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/module/binding
         rebellion/module/phase
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple
         syntax/modread
         syntax/parse
         syntax/parse/define
         syntax/parse/lib/function-header)

;@----------------------------------------------------------------------------------------------------

(define-record-type refactoring-rule (transformer)
  #:omit-root-binding)

(define (refactoring-rule-refactor rule syntax)
  ((refactoring-rule-transformer rule) syntax))

(define-simple-macro
  (define-refactoring-rule id:id parse-option ... [pattern pattern-directive ... replacement])
  (define id
    (constructor:refactoring-rule
     #:transformer
     (syntax-parser
       parse-option ...
       [pattern pattern-directive ... (present #'replacement)]
       [_ absent]))))

(define-refactoring-rule let-to-block
  #:literals (let)
  [(let ([x:id rhs:expr] ...) body:expr ...)
   (block (define x rhs) ... (block body ...))])

(define-refactoring-rule single-block-elimination
  #:literals (block)
  [(block expr) expr])

(define-refactoring-rule immediate-define-block-elimination
  #:literals (define block)
  [(define header:function-header (block body:expr ...))
   (define header body ...)])

(define-refactoring-rule if-always-true
  #:literals (if)
  [(if #true then else) then])

(define-refactoring-rule if-always-false
  #:literals (if)
  [(if #false then else) else])
