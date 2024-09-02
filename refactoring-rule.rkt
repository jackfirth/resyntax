#lang racket/base


(require racket/contract/base)


(provide
 define-refactoring-rule
 define-definition-context-refactoring-rule
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


(require (for-syntax racket/base
                     racket/syntax)
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/type/object
         resyntax/default-recommendations/private/definition-context
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


(define-syntax-parse-rule
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


(define-syntax-parse-rule
  (define-definition-context-refactoring-rule id:id
    #:description (~var description (expr/c #'string?))
    parse-option ...
    [splicing-pattern pattern-directive ... (splicing-replacement ...)])

  ;; These identifiers are macro-introduced, but we use format-id on them anyway so that the expanded
  ;; code is more readable and it's clearer which refactoring rule these syntax classes are derived
  ;; from.
  #:with body-matching-id (format-id #'macro-introduced-context "body-matching-~a" #'id)
  #:with expression-matching-id (format-id #'macro-introduced-context "expression-matching-~a" #'id)

  (begin

    (define-splicing-syntax-class body-matching-id
      #:attributes ([refactored 1])
      parse-option ...
      (pattern splicing-pattern
        pattern-directive ...
        #:with (refactored (... ...)) #'(splicing-replacement ...)))

    (define-syntax-class expression-matching-id
      #:attributes (refactored)

      (pattern (header:header-form-allowing-internal-definitions (~var body body-matching-id))
        #:cut
        #:with refactored #'(header.original (... ...) body.refactored (... ...)))

      (pattern (branching-header:branching-form-allowing-internal-definitions-within-clauses
                clause-before (... ...)
                (~and original-clause [clause-header (~var body body-matching-id)])
                clause-after (... ...))
        #:cut
        #:with refactored
        #'(branching-header.original
           (... ...)
           clause-before (... ...)
           (~replacement [clause-header body.refactored (... ...)] #:original original-clause)
           clause-after (... ...))))

    (define-refactoring-rule id
      #:description description
      [(~var expression expression-matching-id) expression.refactored])))
