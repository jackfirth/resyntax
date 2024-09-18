#lang racket/base


(require racket/contract/base)


(provide
 ~replacement
 ~splicing-replacement
 ~focus-replacement-on
 define-refactoring-suite
 define-refactoring-rule
 define-definition-context-refactoring-rule
 (contract-out
  [refactoring-rule? (-> any/c boolean?)]
  [refactoring-rule-description (-> refactoring-rule? immutable-string?)]
  [refactoring-suite? (-> any/c boolean?)]
  [refactoring-suite
   (->* ()
        (#:rules (sequence/c refactoring-rule?) #:name (or/c interned-symbol? #false))
        refactoring-suite?)]
  [refactoring-suite-rules (-> refactoring-suite? (listof refactoring-rule?))]))


(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor
     (-> refactoring-rule? syntax? source? (option/c syntax-replacement?))])))


(require (for-syntax racket/base racket/syntax resyntax/private/more-syntax-parse-classes)
         racket/sequence
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/symbol
         rebellion/type/object
         resyntax/default-recommendations/private/definition-context
         resyntax/private/source
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-replacement
         syntax/parse
         syntax/parse/define
         syntax/parse/experimental/template)


;@----------------------------------------------------------------------------------------------------


(define-template-metafunction (~replacement stx)
  (syntax-parse stx
    [(_ new-stx #:original orig-syntax)
     (syntax-property #'new-stx 'replacement-for #'orig-syntax)]
    [(_ new-stx #:original-splice (first-orig orig-syntax ... last-orig))
     (syntax-property (syntax-property #'new-stx 'head-replacement-for #'first-orig)
                      'tail-replacement-for #'last-orig)]
    [(_ new-stx #:original-splice (only-orig-syntax))
     (syntax-property (syntax-property #'new-stx 'head-replacement-for #'only-orig-syntax)
                      'tail-replacement-for #'only-orig-syntax)]))


(define-template-metafunction (~splicing-replacement stx)
  (syntax-parse stx
    [(_ (~and new-stx (first-subform subform ... last-subform)) #:original orig-syntax)
     (define first-with-prop (syntax-property #'first-subform 'head-replacement-for #'orig-syntax))
     (define last-with-prop (syntax-property #'last-subform 'tail-replacement-for #'orig-syntax))
     (define new-stx-with-subform-props
       (datum->syntax #'new-stx
                      #`(#,first-with-prop subform ... #,last-with-prop)
                      #'new-stx
                      #'new-stx))
     (syntax-property new-stx-with-subform-props 'replacement-for #'orig-syntax)]
    [(_ (~and new-stx (only-subform)) #:original orig-syntax)
     (define subform-with-props
       (syntax-property (syntax-property #'only-subform 'head-replacement-for #'orig-syntax)
                        'tail-replacement-for
                        #'orig-syntax))
     (define new-stx-with-subform-props
       (datum->syntax #'new-stx #`(#,subform-with-props) #'new-stx #'new-stx))
     (syntax-property new-stx-with-subform-props 'replacement-for #'orig-syntax)]
    [(_ (~and new-stx ()) #:original orig-syntax)
     (syntax-property #'new-stx 'replacement-for #'orig-syntax)]))


(define-template-metafunction (~focus-replacement-on stx)
  (syntax-parse stx
    [(_ (~and new-stx (substx ...)))
     #:cut
     (define substxs-with-prop
       (for/list ([sub (in-list (attribute substx))])
         (syntax-property sub 'focus-replacement-on #true)))
     (syntax-property (datum->syntax #'new-stx substxs-with-prop #'new-stx #'new-stx)
                      'focus-replacement-on #true)]
    [(_ new-stx) (syntax-property #'new-stx 'focus-replacement-on #true)]))


(define-object-type refactoring-rule (transformer description)
  #:omit-root-binding
  #:constructor-name constructor:refactoring-rule)


(define (refactoring-rule-refactor rule syntax source)

  ;; Before refactoring the input syntax, we do two things: create a new scope and add it, and
  ;; traverse the syntax object making a note of each subform's original neighbors. Combined,
  ;; these two things allow us to tell when two neighboring subforms within the output syntax object
  ;; are originally from the input and were originally next to each other in the input. This allows
  ;; Resyntax to preserve any formatting and comments between those two subform when rendering the
  ;; resulting syntax replacement into a string transformation.
  (define rule-introduction-scope (make-syntax-introducer))
  (define prepared-syntax (rule-introduction-scope (syntax-mark-original-neighbors syntax)))

  (option-map
   ((refactoring-rule-transformer rule) prepared-syntax)
   (λ (new-syntax)
     (syntax-replacement
      #:source source
      #:original-syntax syntax
      #:new-syntax (rule-introduction-scope new-syntax)
      #:introduction-scope rule-introduction-scope))))


(define-syntax-parse-rule
  (define-refactoring-rule id:id
    #:description description
    parse-option:syntax-parse-option ...
    pattern
    pattern-directive:syntax-parse-pattern-directive ...
    replacement)
  #:declare description (expr/c #'string?)
  (define id
    (constructor:refactoring-rule
     #:name 'id
     #:description (string->immutable-string description.c)
     #:transformer
     (λ (stx)
       (syntax-parse stx
         (~@ . parse-option) ...
         [pattern (~@ . pattern-directive) ... (present #'replacement)]
         [_ absent])))))


(define-syntax-parse-rule
  (define-definition-context-refactoring-rule id:id
    #:description (~var description (expr/c #'string?))
    parse-option:syntax-parse-option ...
    splicing-pattern
    pattern-directive:syntax-parse-pattern-directive ...
    (splicing-replacement ...))

  ;; These identifiers are macro-introduced, but we use format-id on them anyway so that the expanded
  ;; code is more readable and it's clearer which refactoring rule these syntax classes are derived
  ;; from.
  #:with body-matching-id (format-id #'macro-introduced-context "body-matching-~a" #'id)
  #:with expression-matching-id (format-id #'macro-introduced-context "expression-matching-~a" #'id)

  (begin

    (define-splicing-syntax-class body-matching-id
      #:attributes ([refactored 1])
      (~@ . parse-option) ...
      (pattern splicing-pattern
        (~@ . pattern-directive) ...
        #:with (refactored (... ...)) #'(splicing-replacement ...)))

    (define-syntax-class expression-matching-id
      #:attributes (refactored)

      (pattern ((~var header header-form-allowing-internal-definitions) (~var body body-matching-id))
        #:cut
        #:with refactored #'(header.original (... ...) body.refactored (... ...)))

      (pattern ((~var branching-header branching-form-allowing-internal-definitions-within-clauses)
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
      (~var expression expression-matching-id)
      expression.refactored)))


(define-object-type refactoring-suite (rules)
  #:constructor-name constructor:refactoring-suite
  #:omit-root-binding)


(define (refactoring-suite #:rules [rules '()] #:name [name #false])
  (constructor:refactoring-suite #:rules (sequence->list rules) #:name name))


(begin-for-syntax

  (define-splicing-syntax-class rules-list
    #:attributes (as-list-expr)
    (pattern (~seq) #:with as-list-expr #'(list))
    (pattern (~seq #:rules ~! (rule ...))
      #:declare rule (expr/c #'refactoring-rule?)
      #:with as-list-expr #'(list rule.c ...)))

  (define-splicing-syntax-class suites-list
    #:attributes (as-list-expr)
    (pattern (~seq) #:with as-list-expr #'(list))
    (pattern (~seq #:suites ~! (suite ...))
      #:declare suite (expr/c #'refactoring-suite?)
      #:with as-list-expr #'(append (refactoring-suite-rules suite.c) ...))))


(define-syntax-parse-rule (define-refactoring-suite id:id rules:rules-list suites:suites-list)
  (define id
    (refactoring-suite
     #:name 'id
     #:rules (append rules.as-list-expr suites.as-list-expr))))
