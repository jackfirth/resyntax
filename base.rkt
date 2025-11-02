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
  [refactoring-rule-analyzers (-> refactoring-rule? (listof expansion-analyzer?))]
  [refactoring-suite? (-> any/c boolean?)]
  [refactoring-suite
   (->* ()
        (#:rules (sequence/c refactoring-rule?) #:name (or/c interned-symbol? #false))
        refactoring-suite?)]
  [refactoring-suite-rules (-> refactoring-suite? (listof refactoring-rule?))]
  [refactoring-suite-analyzers (-> refactoring-suite? (listof expansion-analyzer?))]))


(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor
     (-> refactoring-rule? syntax? source? (option/c syntax-replacement?))])))


(require (for-syntax racket/base
                     racket/list
                     racket/syntax
                     resyntax/private/more-syntax-parse-classes)
         racket/list
         racket/sequence
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/symbol
         rebellion/type/object
         resyntax/default-recommendations/analyzers/identifier-usage
         resyntax/default-recommendations/analyzers/ignored-result-values
         resyntax/default-recommendations/analyzers/variable-mutability
         resyntax/default-recommendations/private/definition-context
         resyntax/private/analyzer
         resyntax/private/logger
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


(define-object-type refactoring-rule (transformer description uses-universal-tagged-syntax? analyzers)
  #:omit-root-binding
  #:constructor-name constructor:refactoring-rule)


(define (refactoring-rule-refactor rule syntax source)

  ;; Before refactoring the input syntax, we create a new scope and add it. Combined with the code in
  ;; resyntax/private/source which marks the original path of every syntax object before expansion,
  ;; this allows us to tell when two neighboring subforms within the output syntax object are
  ;; originally from the input and were originally next to each other in the input. This allows
  ;; Resyntax to preserve any formatting and comments between those two subform when rendering the
  ;; resulting syntax replacement into a string transformation.
  (define rule-introduction-scope (make-syntax-introducer))
  (define prepared-syntax (rule-introduction-scope syntax))

  (option-map
   ((refactoring-rule-transformer rule) prepared-syntax)
   (λ (new-syntax)
     (syntax-replacement
      #:source source
      #:original-syntax syntax
      #:new-syntax (rule-introduction-scope new-syntax)
      #:introduction-scope rule-introduction-scope
      #:uses-universal-tagged-syntax? (refactoring-rule-uses-universal-tagged-syntax? rule)))))


(define-syntax-parse-rule
  (define-refactoring-rule id:id
    #:description description
    (~optional (~seq #:uses-universal-tagged-syntax? uses-universal-tagged-syntax?))
    parse-option:syntax-parse-option ...
    pattern
    pattern-directive:syntax-parse-pattern-directive ...
    replacement)
  #:declare description (expr/c #'string?)

  #:attr partial-match-log-statement
  (and (not (empty? (attribute pattern-directive)))
       #'(log-resyntax-debug "~a: partial match on line ~a" 'id (syntax-line this-syntax)))
  #:with (wrapped-pattern-directive ...)
  (for/list ([directive (in-list (attribute pattern-directive))])
    (syntax-parse directive
      [(#:when condition:expr) #'(#:when (log-resyntax-rule-condition condition))]
      [_ directive]))

  (define id
    (constructor:refactoring-rule
     #:name 'id
     #:description (string->immutable-string description.c)
     #:uses-universal-tagged-syntax? (~? uses-universal-tagged-syntax? #false)
     #:analyzers (list identifier-usage-analyzer
                       ignored-result-values-analyzer
                       variable-mutability-analyzer)
     #:transformer
     (λ (stx)
       (syntax-parse stx
         (~@ . parse-option) ...
         [pattern
           (~? (~@ #:do [partial-match-log-statement]))
           (~@ . wrapped-pattern-directive) ... (present #'replacement)]
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

  #:attr log-statement
  (and (not (empty? (attribute pattern-directive)))
       #'(log-resyntax-debug "~a: partial match" 'id))

  #:with (wrapped-pattern-directive ...)
  (for/list ([directive (in-list (attribute pattern-directive))])
    (syntax-parse directive
      [(#:when condition:expr) #'(#:when (log-resyntax-rule-condition condition))]
      [_ directive]))

  (begin

    (define-splicing-syntax-class body-matching-id
      #:attributes ([refactored 1])
      (~@ . parse-option) ...
      (pattern splicing-pattern
        (~? (~@ #:do [log-statement]))
        (~@ . wrapped-pattern-directive) ...
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


(define-object-type refactoring-suite (rules analyzers)
  #:constructor-name constructor:refactoring-suite
  #:omit-root-binding)


(define (refactoring-suite #:rules [rules '()] #:name [name #false])
  (define rule-list (sequence->list rules))
  (define combined-analyzers
    (remove-duplicates
     (append-map refactoring-rule-analyzers rule-list)))
  (constructor:refactoring-suite #:rules rule-list #:analyzers combined-analyzers #:name name))


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


(module+ test
  (require rackunit
           resyntax/private/analyzer)

  (test-case "refactoring-rule stores analyzers"
    (define-refactoring-rule test-rule
      #:description "test rule"
      pattern
      replacement)
    
    (check-true (refactoring-rule? test-rule))
    (check-equal? (length (refactoring-rule-analyzers test-rule)) 3)
    (check-true (andmap expansion-analyzer? (refactoring-rule-analyzers test-rule))))

  (test-case "refactoring-suite combines analyzers from rules"
    (define-refactoring-rule rule1
      #:description "rule 1"
      pattern1
      replacement1)
    
    (define-refactoring-rule rule2
      #:description "rule 2"
      pattern2
      replacement2)
    
    (define suite (refactoring-suite #:rules (list rule1 rule2)))
    
    (check-true (refactoring-suite? suite))
    (check-equal? (length (refactoring-suite-rules suite)) 2)
    ;; All rules have the same analyzers, so the combined list should have 3 unique analyzers
    (check-equal? (length (refactoring-suite-analyzers suite)) 3)
    (check-true (andmap expansion-analyzer? (refactoring-suite-analyzers suite))))

  (test-case "nested suites combine analyzers correctly"
    (define-refactoring-rule inner-rule
      #:description "inner rule"
      inner-pattern
      inner-replacement)
    
    (define inner-suite (refactoring-suite #:rules (list inner-rule)))
    
    (define-refactoring-rule outer-rule
      #:description "outer rule"
      outer-pattern
      outer-replacement)
    
    (define outer-suite (refactoring-suite #:rules (list outer-rule inner-rule)))
    
    (check-equal? (length (refactoring-suite-analyzers inner-suite)) 3)
    ;; Both rules have the same analyzers, so deduplicated should still be 3
    (check-equal? (length (refactoring-suite-analyzers outer-suite)) 3))

  (test-case "define-refactoring-suite with nested suites preserves analyzers"
    (define-refactoring-rule rule-a
      #:description "Rule A"
      pattern-a
      replacement-a)

    (define-refactoring-suite suite-a
      #:rules (rule-a))

    (define-refactoring-rule rule-b
      #:description "Rule B"
      pattern-b
      replacement-b)

    (define-refactoring-suite suite-b
      #:rules (rule-b)
      #:suites (suite-a))

    ;; Suite B should have both rules
    (check-equal? (length (refactoring-suite-rules suite-b)) 2)
    ;; And should have 3 analyzers (deduplicated from both rules)
    (check-equal? (length (refactoring-suite-analyzers suite-b)) 3)
    (check-true (andmap expansion-analyzer? (refactoring-suite-analyzers suite-b)))))
