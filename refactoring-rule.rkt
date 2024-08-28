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
         resyntax/private/syntax-neighbors
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define current-source-code-analysis (make-parameter #false #false 'current-source-code-analysis))


(define-object-type refactoring-rule (transformer description)
  #:omit-root-binding
  #:constructor-name constructor:refactoring-rule)


(define (refactoring-rule-refactor rule syntax #:analysis analysis)

  ;; Before refactoring the input syntax, we do two things: create a new scope and add it, and
  ;; traverse the syntax object making a note of each subform's original neighbors. Combined,
  ;; these two things allow us to tell when two neighboring subforms within the output syntax object
  ;; are originally from the input and were originally next to each other in the input. This allows
  ;; Resyntax to preserve any formatting and comments between those two subform when rendering the
  ;; resulting syntax replacement into a string transformation.
  (define rule-introduction-scope (make-syntax-introducer))
  (define prepared-syntax (rule-introduction-scope (syntax-mark-original-neighbors syntax)))

  (option-map
   ((refactoring-rule-transformer rule) prepared-syntax analysis)
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
