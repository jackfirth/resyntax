#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [conditional-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         rebellion/type/record
         rebellion/private/static-name
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
  #:description "This if-else chain can be converted to a cond expression."
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
  #:attributes (when-or-unless condition [body 1])
  #:literals (if void not begin let)

  (pattern (if (not condition) (void) :block-expression) #:with when-or-unless #'when)
  (pattern (if (not condition) :block-expression (void)) #:with when-or-unless #'unless)
  (pattern (if condition (void) :block-expression) #:with when-or-unless #'unless)
  (pattern (if condition :block-expression (void)) #:with when-or-unless #'when))


(define-refactoring-rule if-void-to-when-or-unless
  #:description equivalent-conditional-description
  [conditional:when-or-unless-equivalent-conditional
   (conditional.when-or-unless conditional.condition NEWLINE (ORIGINAL-SPLICE conditional.body ...))])


(define conditional-shortcuts
  (refactoring-suite
   #:name (name conditional-shortcuts)
   #:rules
   (list if-else-false-to-and
         if-void-to-when-or-unless
         if-x-else-x-to-and
         nested-if-to-cond)))
