#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [conditional-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/definition-context
         resyntax/default-recommendations/private/exception
         resyntax/default-recommendations/private/metafunction
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class nested-if-else
  #:attributes ([branch 1] branches-size)
  #:literals (if)

  (pattern (if cond then nested:nested-if-else)
    #:with (branch ...) #'(NEWLINE [cond then] nested.branch ...)
    #:attr branches-size (+ 1 (attribute nested.branches-size)))

  (pattern (if cond then default)
    #:with (branch ...) #'(NEWLINE [cond then] NEWLINE [else default])
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
    conditional.condition NEWLINE (ORIGINAL-SPLICE conditional.body ...))])


(define-refactoring-rule always-throwing-if-to-when
  #:description "Using `when` and `unless` is simpler than a conditional with an always-throwing branch."
  #:literals (if)
  [(header:header-form-allowing-internal-definitions
    (if condition:condition-expression
      fail:always-throwing-expression
      else-expression))
   (header.formatted
    ... NEWLINE
    ((~if condition.negated? unless when) condition.base-condition NEWLINE fail) NEWLINE
    else-expression)])


(define-refactoring-rule always-throwing-cond-to-when
  #:description "Using `when` and `unless` is simpler than a conditional with an always-throwing branch."
  #:literals (cond)
  [(header:header-form-allowing-internal-definitions
    (cond
      [condition:condition-expression
       fail:always-throwing-expression]
      [else
       body ...]))
   (header.formatted
    ... NEWLINE
    ((~if condition.negated? unless when) condition.base-condition NEWLINE fail) NEWLINE
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


(define conditional-shortcuts
  (refactoring-suite
   #:name (name conditional-shortcuts)
   #:rules
   (list always-throwing-cond-to-when
         always-throwing-if-to-when
         cond-else-cond-to-cond
         if-else-false-to-and
         if-void-to-when-or-unless
         if-x-else-x-to-and
         nested-if-to-cond)))
