#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [conditional-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         resyntax/base
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/exception
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/default-recommendations/private/metafunction
         resyntax/default-recommendations/private/syntax-equivalence
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class nested-if-else
  #:attributes ([branch 1] branches-size)
  #:literals (if)

  (pattern (if cond then nested:nested-if-else)
    #:with (branch ...) #'([cond then] nested.branch ...)
    #:attr branches-size (+ 1 (attribute nested.branches-size)))

  (pattern (if cond then default)
    #:with (branch ...) #'([cond then] [else default])
    #:attr branches-size 2))


(define-refactoring-rule nested-if-to-cond
  #:description "This `if`-`else` chain can be converted to a `cond` expression."
  #:literals (cond)
  nested:nested-if-else
  #:when (> (attribute nested.branches-size) 2)
  (cond nested.branch ...))


(define equivalent-conditional-description
  "This conditional expression can be replaced with a simpler, equivalent expression.")


(define-refactoring-rule if-else-false-to-and
  #:description equivalent-conditional-description
  #:literals (if and)
  (if (~or (and condition-part:expr ...) condition:expr) then-branch #false)  
  (and (~? (~@ condition-part ...) condition) then-branch))


(define-refactoring-rule if-x-else-x-to-and
  #:description equivalent-conditional-description
  #:literals (if and)
  (if x:id (~or (and then-part ...) then-branch:expr) y:id)
  #:when (free-identifier=? (attribute x) (attribute y))
  (and x (~? (~@ then-part ...) then-branch)))


(define-syntax-class block-expression
  #:attributes ([body 1])
  #:literals (begin let)
  (pattern (begin body ...))
  (pattern (let () body ...))
  (pattern single-body #:with (body ...) #'(single-body)))


(define-syntax-class when-or-unless-equivalent-if-expression
  #:attributes (negated? condition [body 1])
  #:literals (if void not begin let)

  (pattern (if (not condition) (void) :block-expression) #:with negated? #false)
  (pattern (if (not condition) :block-expression (void)) #:with negated? #true)
  (pattern (if condition (void) :block-expression) #:with negated? #true)
  (pattern (if condition :block-expression (void)) #:with negated? #false))


(define-refactoring-rule if-void-to-when-or-unless
  #:description equivalent-conditional-description
  conditional:when-or-unless-equivalent-if-expression
  ((~if conditional.negated? unless when) conditional.condition conditional.body ...))


(define-syntax-class when-or-unless-equivalent-cond-expression
  #:attributes (negated? condition [body 1])
  #:literals (cond void not begin let)

  (pattern (cond [(not condition) (void)] [else :block-expression]) #:with negated? #false)
  (pattern (cond [(not condition) :block-expression] [else (void)]) #:with negated? #true)
  (pattern (cond [condition (void)] [else :block-expression]) #:with negated? #true)
  (pattern (cond [condition :block-expression] [else (void)]) #:with negated? #false))


(define-refactoring-rule cond-void-to-when-or-unless
  #:description equivalent-conditional-description
  conditional:when-or-unless-equivalent-cond-expression
  ((~if conditional.negated? unless when) conditional.condition conditional.body ...))


(define-syntax-class always-throwing-if-expression
  #:attributes (equivalent-guard-expression success-expression)
  #:literals (if)
  (pattern (if condition:condition-expression
               fail:always-throwing-expression
               success-expression)
    #:with equivalent-guard-expression
    #'((~if condition.negated? unless when) condition.base-condition fail))
  (pattern (if condition:condition-expression
               success-expression
               fail:always-throwing-expression)
    #:with equivalent-guard-expression
    #'((~if condition.negated? when unless) condition.base-condition fail)))


(define-definition-context-refactoring-rule always-throwing-if-to-when
  #:description
  "Using `when` and `unless` is simpler than a conditional with an always-throwing branch."
  (~seq body-before ... throwing-if:always-throwing-if-expression)
  (body-before ...
   (~@ . (~focus-replacement-on
          (~splicing-replacement (throwing-if.equivalent-guard-expression
                                  throwing-if.success-expression)
                                 #:original throwing-if)))))


(define-syntax-class always-throwing-cond-expression
  #:attributes ([guard-expression 1] [body 1])
  #:literals (cond)
  (pattern (cond
             [condition:condition-expression fail:always-throwing-expression] ...+
             [else body ...])
    #:with (guard-expression ...)
    #'(((~if condition.negated? unless when) condition.base-condition fail) ...)))


(define-definition-context-refactoring-rule always-throwing-cond-to-when
  #:description
  "Using `when` and `unless` is simpler than a conditional with an always-throwing branch."
  (~seq body-before ... throwing-cond:always-throwing-cond-expression)
  (body-before ...
   (~@ . (~focus-replacement-on
          (~splicing-replacement (throwing-cond.guard-expression ...
                                  throwing-cond.body ...)
                                 #:original throwing-cond)))))


(define-definition-context-refactoring-rule throw-unless-truthy-to-or
  #:description
  "Instead of using `unless` to throw an exception if a truthy value is false, consider using `or`."
  #:literals (define unless)
  (~seq body-before ...
        (~and def (define truthy-var:id truthy-expr))
        (~and check (unless truthy-var2:id fail:always-throwing-expression))
        truthy-var3:id)

  #:when (free-identifier=? #'truthy-var #'truthy-var2)
  #:when (free-identifier=? #'truthy-var #'truthy-var3)
  #:with or-expression
  #'(~replacement (or truthy-expr fail) #:original-splice (def check truthy-var3))
  #:with focused-or-expression
  (if (empty? (attribute body-before))
      #'or-expression
      #'(~focus-replacement-on or-expression))

  (body-before ... focused-or-expression))


(define-refactoring-rule cond-else-cond-to-cond
  #:description
  "The `else` clause of this `cond` expression is another `cond` expression and can be flattened."
  #:literals (cond else)
  (cond-id:cond
   clause ...
   (~and outer-else-clause [else (cond nested-clause ...)]))
  (cond-id
   clause ...
   (~@ . (~splicing-replacement (nested-clause ...) #:original outer-else-clause))))


(define-syntax-class let-refactorable-cond-clause
  #:attributes (refactored)
  (pattern [condition:expr leading-body ... let-expr:refactorable-let-expression]
    #:with refactored
    #`(~replacement
       [condition leading-body ... (~@ . (~focus-replacement-on (let-expr.refactored ...)))]
       #:original #,this-syntax)))


(define-refactoring-rule cond-let-to-cond-define
  #:description
  "Internal definitions are recommended instead of `let` expressions, to reduce nesting."
  #:literals (cond)
  (cond-id:cond clause-before ... clause:let-refactorable-cond-clause clause-after ...)
  (cond-id clause-before ... clause.refactored clause-after ...))


(define-syntax-class if-arm
  #:attributes (uses-begin? uses-let? [refactored 1])
  #:literals (begin)

  (pattern (begin body ...)
    #:attr uses-begin? #true
    #:attr uses-let? #false
    #:with (refactored ...) #`(~splicing-replacement (body ...) #:original #,this-syntax))

  (pattern :refactorable-let-expression
    #:attr uses-begin? #false
    #:attr uses-let? #true)

  (pattern other
    #:with (refactored ...) #'(other)
    #:attr uses-begin? #false
    #:attr uses-let? #false))


(define-refactoring-rule if-begin-to-cond
  #:description "Using `cond` instead of `if` here makes `begin` unnecessary"
  #:literals (if void)
  (if condition
      (~and then-expr:if-arm (~not (void)))
      (~and else-expr:if-arm (~not (void))))
  #:when (or (attribute then-expr.uses-begin?) (attribute else-expr.uses-begin?))
  (cond (~replacement [condition then-expr.refactored ...] #:original-splice (condition then-expr))
        (~replacement [else else-expr.refactored ...] #:original else-expr)))


(define-refactoring-rule if-let-to-cond
  #:description
  "`cond` with internal definitions is preferred over `if` with `let`, to reduce nesting"
  #:literals (if void)
  (if condition
      (~and then-expr:if-arm (~not (void)))
      (~and else-expr:if-arm (~not (void))))
  #:when (or (attribute then-expr.uses-let?) (attribute else-expr.uses-let?))
  (cond (~replacement [condition then-expr.refactored ...] #:original-splice (condition then-expr))
        (~replacement [else else-expr.refactored ...] #:original else-expr)))


(define-refactoring-rule and-let-to-cond
  #:description
  "Using `cond` allows converting `let` to internal definitions, reducing nesting"
  #:literals (and cond)
  (and condition let-expr:refactorable-let-expression)
  #:when (not (empty? (attribute let-expr.id)))
  (cond [condition let-expr.refactored ...]
        [else #false]))


(define-refactoring-rule nested-when-to-compound-when
  #:description
  "Nested `when` expressions can be merged into a single compound `when` expression."
  #:literals (when and)
  (when outer-condition:expr (when inner-condition:expr body:expr ...))
  (when (and outer-condition inner-condition) body ...))


(define-refactoring-rule ignored-and-to-when
  #:description "This `and` expression's result is ignored. Using `when` makes this clearer."
  #:literals (and)
  (and condition:expr body:expr)
  #:when (syntax-property this-syntax 'expression-result)
  #:when (equal? (syntax-property this-syntax 'expression-result) 'ignored)
  (when condition body))


(define-refactoring-rule explicit-cond-else-void
  #:description "Add an explicit `[else (void)]` clause to make the default behavior clear."
  #:literals (cond else void)
  (cond-id:cond (~and clause (~not [else . _])) ...)
  #:when (equal? (syntax-property this-syntax 'expression-result) 'ignored)
  (cond-id clause ... [else (void)]))


(define-definition-context-refactoring-rule tail-sharing-cond-to-when
  #:description "Both branches of this `cond` expression share the same tail expression. Moving that \
tail expression outside `cond` lets you replace `cond` with `when`."
  #:literals (cond else)
  (~seq body-before ...
        (cond [condition:expr true-body ... true-tail]
              [else false-tail])
        body-after ...)
  #:when (syntax-free-identifier=? (attribute true-tail) (attribute false-tail))
  (body-before ...
   (when condition true-body ...)
   true-tail
   body-after ...))


(define-refactoring-suite conditional-shortcuts
  #:rules (always-throwing-cond-to-when
           always-throwing-if-to-when
           and-let-to-cond
           cond-else-cond-to-cond
           cond-let-to-cond-define
           cond-void-to-when-or-unless
           explicit-cond-else-void
           if-begin-to-cond
           if-else-false-to-and
           if-let-to-cond
           if-void-to-when-or-unless
           if-x-else-x-to-and
           ignored-and-to-when
           nested-if-to-cond
           nested-when-to-compound-when
           tail-sharing-cond-to-when
           throw-unless-truthy-to-or))
