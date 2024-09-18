#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [boolean-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         rebellion/private/static-name
         resyntax/base
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/syntax-lines
         resyntax/default-recommendations/private/syntax-tree
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-refactoring-rule nested-or-to-flat-or
  #:description "Nested `or` expressions can be flattened into a single, equivalent `or` expression."
  (~var or-tree (syntax-tree #'or))
  ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
  ;; any formatting between adjacent leaves.
  #:when (oneline-syntax? #'or-tree)
  #:when (>= (attribute or-tree.rank) 2)
  (or or-tree.leaf ...))


(define-refactoring-rule nested-and-to-flat-and
  #:description
  "Nested `and` expressions can be flattened into a single, equivalent `and` expression."
  (~var and-tree (syntax-tree #'and))
  ;; Restricted to single-line expressions for now because the syntax-tree operations don't preserve
  ;; any formatting between adjacent leaves.
  #:when (oneline-syntax? #'and-tree)
  #:when (>= (attribute and-tree.rank) 2)
  (and and-tree.leaf ...))


(define-refactoring-rule if-then-true-else-false-to-condition
  #:description "The condition of this `if` expression is already a boolean and can be used directly."
  #:literals (if)
  (if condition:likely-boolean #true #false)
  condition)


(define-refactoring-rule if-then-false-else-true-to-not
  #:description "This `if` expression can be refactored to an equivalent expression using `not`."
  #:literals (if)
  (if condition #false #true)
  (not condition))


(define-refactoring-rule if-else-false-to-and
  #:description "This `if` expression can be refactored to an equivalent expression using `and`."
  #:literals (if)
  (if condition then #false)
  (and condition then))


(define-syntax-class negated-condition
  #:attributes (flipped)
  #:literals (not)
  (pattern (not base-condition:expr)
    #:with flipped #`(~replacement base-condition #:original #,this-syntax)))


(define-refactoring-rule inverted-when
  #:description "This negated `when` expression can be replaced by an `unless` expression."
  #:literals (when)
  (when-id:when negated:negated-condition body ...)
  ((~replacement unless #:original when-id) negated.flipped body ...))


(define-refactoring-rule inverted-unless
  #:description "This negated `unless` expression can be replaced by a `when` expression."
  #:literals (unless)
  (unless-id:unless negated:negated-condition body ...)
  ((~replacement when #:original unless-id) negated.flipped body ...))


(define-refactoring-suite boolean-shortcuts
  #:rules (if-then-false-else-true-to-not
           if-then-true-else-false-to-condition
           if-else-false-to-and
           inverted-when
           inverted-unless
           nested-and-to-flat-and
           nested-or-to-flat-or))
