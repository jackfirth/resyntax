#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [conditional-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/private/static-name
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/exception
         resyntax/default-recommendations/private/let-binding
         resyntax/default-recommendations/private/metafunction
         resyntax/default-recommendations/private/syntax-lines
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/private/syntax-replacement
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
  [nested:nested-if-else
   #:when (> (attribute nested.branches-size) 2)
   (cond nested.branch ...)])


(define equivalent-conditional-description
  "This conditional expression can be replaced with a simpler, equivalent expression.")


(define-refactoring-rule if-else-false-to-and
  #:description equivalent-conditional-description
  #:literals (if)
  [(if condition then-branch #false)
   (and (ORIGINAL-SPLICE condition then-branch))])


(define-refactoring-rule if-x-else-x-to-and
  #:description equivalent-conditional-description
  #:literals (if)
  [(if x:id then-branch:expr y:id)
   #:when (free-identifier=? #'x #'y)
   (and (ORIGINAL-SPLICE x then-branch))])


(define-syntax-class block-expression
  #:attributes ([body 1])
  #:literals (begin let)
  (pattern (begin body ...))
  (pattern (let () body ...))
  (pattern single-body #:with (body ...) #'(single-body)))


(define-syntax-class when-or-unless-equivalent-conditional
  #:attributes (negated? condition [body 1])
  #:literals (if void not begin let)

  (pattern (if (not condition) (void) :block-expression) #:with negated? #false)
  (pattern (if (not condition) :block-expression (void)) #:with negated? #true)
  (pattern (if condition (void) :block-expression) #:with negated? #true)
  (pattern (if condition :block-expression (void)) #:with negated? #false))


(define-refactoring-rule if-void-to-when-or-unless
  #:description equivalent-conditional-description
  [conditional:when-or-unless-equivalent-conditional
   ((~if conditional.negated? unless when)
    conditional.condition (ORIGINAL-SPLICE conditional.body ...))])


(define-refactoring-rule always-throwing-if-to-when
  #:description
  "Using `when` and `unless` is simpler than a conditional with an always-throwing branch."
  #:literals (if)
  [(header:header-form-allowing-internal-definitions
    (if condition:condition-expression
        fail:always-throwing-expression
        else-expression))
   (header.formatted
    ...
    ((~if condition.negated? unless when) condition.base-condition fail)
    else-expression)])


(define-refactoring-rule always-throwing-cond-to-when
  #:description
  "Using `when` and `unless` is simpler than a conditional with an always-throwing branch."
  #:literals (cond)
  [(header:header-form-allowing-internal-definitions
    (cond
      [condition:condition-expression
       fail:always-throwing-expression]
      [else
       body ...]))
   (header.formatted
    ...
    ((~if condition.negated? unless when) condition.base-condition fail)
    (ORIGINAL-SPLICE body ...))])


(define-refactoring-rule cond-else-cond-to-cond
  #:description
  "The `else` clause of this `cond` expression is another `cond` expression and can be flattened."
  #:literals (cond else)
  [((~and outer-cond-id cond)
    clause ... last-non-else-clause
    (~and outer-else-clause [else (cond nested-clause ...)]))
   ((ORIGINAL-SPLICE outer-cond-id clause ... last-non-else-clause)
    (ORIGINAL-GAP last-non-else-clause outer-else-clause)
    (ORIGINAL-SPLICE nested-clause ...))])


(define-syntax-class let-refactorable-cond-clause
  #:attributes ([refactored 0])
  (pattern
    [condition:expr let-expr:body-with-refactorable-let-expression]
    #:with refactored #'[condition let-expr.refactored ...]))


(define (first-syntax stx)
  (define as-list (syntax->list stx))
  (and as-list (not (empty? as-list)) (first as-list)))


(define (last-syntax stx)
  (define as-list (syntax->list stx))
  (and as-list (not (empty? as-list)) (last as-list)))


(define-refactoring-rule cond-let-to-cond-define
  #:description
  "Internal definitions are recommended instead of `let` expressions, to reduce nesting."
  #:literals (cond)
  [((~and outer-cond-id cond)
    clause-before ...
    clause:let-refactorable-cond-clause
    clause-after ...)
   #:with form-before (or (last-syntax #'(clause-before ...)) #'outer-cond-id)
   #:with (after ...)
   (let ([form-after (first-syntax #'(clause-after ...))])
     (if form-after
         #`((ORIGINAL-GAP clause #,form-after) (ORIGINAL-SPLICE clause-after ...))
         (list)))
   ((ORIGINAL-SPLICE outer-cond-id clause-before ...)
    (ORIGINAL-GAP form-before clause)
    clause.refactored
    after ...)])


(define-syntax-class if-arm
  #:attributes (uses-begin? uses-let? [refactored 1])
  #:literals (begin)
  (pattern (begin body ...)
    #:attr uses-begin? #true
    #:attr uses-let? #false
    #:with (refactored ...) #'((ORIGINAL-SPLICE body ...)))
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
  [(if condition
       (~and then-expr:if-arm (~not (void)))
       (~and else-expr:if-arm (~not (void))))
   #:when (or (attribute then-expr.uses-begin?) (attribute else-expr.uses-begin?))
   #:with (true-branch ...)
   (if (attribute then-expr.uses-begin?)
       #'([condition (ORIGINAL-GAP condition then-expr) then-expr.refactored ...])
       #'([condition then-expr.refactored ...]))
   #:with (false-branch ...)
   (if (attribute else-expr.uses-begin?)
       #'([else (ORIGINAL-GAP then-expr else-expr) else-expr.refactored ...])
       #'((ORIGINAL-GAP then-expr else-expr) [else else-expr.refactored ...]))
   (cond
     true-branch ...
     false-branch ...)])


(define-refactoring-rule if-let-to-cond
  #:description
  "`cond` with internal definitions is preferred over `if` with `let`, to reduce nesting"
  #:literals (if void)
  [(if condition
       (~and then-expr:if-arm (~not (void)))
       (~and else-expr:if-arm (~not (void))))
   #:when (or (attribute then-expr.uses-let?) (attribute else-expr.uses-let?))
   #:with (true-branch ...)
   (if (attribute then-expr.uses-let?)
       #'([condition (ORIGINAL-GAP condition then-expr) then-expr.refactored ...])
       #'([condition then-expr.refactored ...]))
   #:with (false-branch ...)
   (if (attribute else-expr.uses-let?)
       #'([else (ORIGINAL-GAP then-expr else-expr) else-expr.refactored ...])
       #'((ORIGINAL-GAP then-expr else-expr) [else else-expr.refactored ...]))
   (cond
     true-branch ...
     false-branch ...)])


(define conditional-shortcuts
  (refactoring-suite
   #:name (name conditional-shortcuts)
   #:rules
   (list always-throwing-cond-to-when
         always-throwing-if-to-when
         cond-else-cond-to-cond
         cond-let-to-cond-define
         if-begin-to-cond
         if-else-false-to-and
         if-let-to-cond
         if-void-to-when-or-unless
         if-x-else-x-to-and
         nested-if-to-cond)))
