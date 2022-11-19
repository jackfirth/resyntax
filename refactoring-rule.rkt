#lang racket/base


(require racket/contract/base)


(provide
 define-refactoring-rule
 (contract-out
  [refactoring-rule? predicate/c]
  [refactoring-rule-description (-> refactoring-rule? immutable-string?)]
  [current-source-code-analysis (-> (or/c source-code-analysis? #false))]))


(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor
     (-> refactoring-rule? syntax? #:analysis source-code-analysis?
         (option/c syntax-replacement?))])))


(require (for-syntax racket/base)
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/type/object
         resyntax/private/source
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define current-source-code-analysis (make-parameter #false #false 'current-source-code-analysis))


(define-object-type refactoring-rule (transformer description)
  #:omit-root-binding
  #:constructor-name constructor:refactoring-rule)


(define (refactoring-rule-refactor rule syntax #:analysis analysis)
  (define rule-introduction-scope (make-syntax-introducer))
  (option-map
   ((refactoring-rule-transformer rule) (rule-introduction-scope syntax) analysis)
   (λ (new-syntax)
     (syntax-replacement
      #:original-syntax syntax
      #:new-syntax (rule-introduction-scope new-syntax)
      #:introduction-scope rule-introduction-scope))))


(define-simple-macro
  (define-refactoring-rule id:id
    #:description description
    parse-option ...
    [pattern pattern-directive ... replacement])
  #:declare description (expr/c #'string?)
  (define id
    (constructor:refactoring-rule
     #:name 'id
     #:description (string->immutable-string description.c)
     #:transformer
     (λ (stx analysis)
       (parameterize ([current-source-code-analysis analysis])
         (syntax-parse stx
           parse-option ...
           [pattern pattern-directive ... (present #'replacement)]
           [_ absent]))))))
