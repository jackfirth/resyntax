#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [miscellaneous-suggestions refactoring-suite?]))


(require (for-syntax racket/base)
         racket/match
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define if-begin-to-cond-message
  "The `cond` form supports multiple body expressions in each branch, making `begin` unnecessary.")

(define-refactoring-rule if-then-begin-to-cond
  #:description if-begin-to-cond-message
  #:literals (if begin)
  [(if condition (begin then-body ...) else-branch)
   (cond
     NEWLINE [condition (~@ NEWLINE then-body) ...]
     NEWLINE [else NEWLINE else-branch])])


(define-refactoring-rule if-else-begin-to-cond
  #:description if-begin-to-cond-message
  #:literals (if begin)
  [(if condition then-branch (begin else-body ...))
   (cond
     NEWLINE [condition NEWLINE then-branch]
     NEWLINE [else (~@ NEWLINE else-body) ...])])


(define-refactoring-rule if-else-cond-to-cond
  #:description if-begin-to-cond-message
  #:literals (if cond)
  [(if condition then-branch (cond clause ...))
   (cond
     NEWLINE [condition NEWLINE then-branch]
     (~@ NEWLINE clause) ...)])


(define-refactoring-rule cond-else-if-to-cond
  #:description "The `else`-`if` branch of this `cond` expression can be collapsed into the `cond`\
 expression."
  #:literals (cond else if)
  [(cond clause ... [else (if inner-condition inner-then-branch else-branch)])
   (cond
     (~@ NEWLINE clause) ...
     NEWLINE [inner-condition NEWLINE inner-then-branch]
     NEWLINE [else NEWLINE else-branch])])


(define-refactoring-rule cond-begin-to-cond
  #:description "The bodies of `cond` clauses are already implicitly wrapped in `begin`."
  #:literals (cond begin)
  [(cond clause-before ...
         [condition (begin body ...)]
         clause-after ...)
   (cond
     (~@ NEWLINE clause-before) ...
     NEWLINE [condition (~@ NEWLINE body) ...]
     (~@ NEWLINE clause-after) ...)])


(define-refactoring-rule or-cond-to-cond
  #:description "This `or` expression can be turned into a clause of the inner `cond` expression,\
 reducing nesting."
  #:literals (or cond)
  [(or condition (cond clause ...))
   (cond
     NEWLINE [condition #t]
     (~@ NEWLINE clause) ...)])


(define-refactoring-rule and-match-to-match
  #:description "This `and` expression can be turned into a clause of the inner `match` expression,\
 reducing nesting."
  #:literals (and match)
  [(and and-subject:id (match match-subject:id match-clause ...))
   #:when (free-identifier=? #'and-subject #'match-subject)
   (match match-subject
     NEWLINE [#false #false]
     (~@ NEWLINE match-clause) ...)])


(define miscellaneous-suggestions
  (refactoring-suite
   #:name (name miscellaneous-suggestions)
   #:rules (list and-match-to-match
                 cond-begin-to-cond
                 cond-else-if-to-cond
                 if-then-begin-to-cond
                 if-else-begin-to-cond
                 if-else-cond-to-cond
                 or-cond-to-cond)))
