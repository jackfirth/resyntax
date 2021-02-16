#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [miscellaneous-suggestions refactoring-suite?]))


(require (for-syntax racket/base)
         racket/match
         rebellion/private/guarded-block
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define/guard (free-identifiers=? ids other-ids)
  (define id-list (syntax->list ids))
  (define other-id-list (syntax->list other-ids))
  (guard (equal? (length id-list) (length other-id-list)) else
    #false)
  (for/and ([id (in-list id-list)] [other-id (in-list other-id-list)])
    (free-identifier=? id other-id)))


(define-refactoring-rule define-lambda-to-define
  #:description "The define form supports a shorthand for defining functions."
  #:literals (define lambda)
  [(define header (lambda formals body ...))
   (define (header . formals) (~@ NEWLINE body) ...)])


(define-refactoring-rule define-case-lambda-to-define
  #:description "This use of case-lambda is equivalent to using define with optional arguments."
  #:literals (define case-lambda)
  [(define id:id
     (case-lambda
       [(case1-arg:id ...)
        (usage:id usage1:id ... default:expr)]
       [(case2-arg:id ... bonus-arg:id)
        body ...]))
   #:when (free-identifier=? #'id #'usage)
   #:when (free-identifiers=? #'(case1-arg ...) #'(case2-arg ...))
   #:when (free-identifiers=? #'(case1-arg ...) #'(usage1 ...))
   (define (id case2-arg ... [bonus-arg default])
     (~@ NEWLINE body) ...)])


(define if-begin-to-cond-message
  "The cond form supports multiple body expressions in each branch, making begin unnecessary.")

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


(define-refactoring-rule if-else-if-to-cond
  #:description "These nested ifs can be collapsed into a single cond expression."
  #:literals (if)
  [(if condition then-branch (if inner-condition inner-then-branch else-branch))
   (cond
     NEWLINE [condition NEWLINE then-branch]
     NEWLINE [inner-condition NEWLINE inner-then-branch]
     NEWLINE [else else-branch])])


(define-refactoring-rule if-x-else-x-to-and
  #:description "This if expression can be replaced with an equivalent and expression."
  #:literals (if)
  [(if x:id then-branch:expr y:id)
   #:when (free-identifier=? #'x #'y)
   (and x then-branch)])


(define-refactoring-rule cond-else-if-to-cond
  #:description "The else if branch of this cond expression can be collapsed into the cond\
 expression."
  #:literals (cond else if)
  [(cond clause ... [else (if inner-condition inner-then-branch else-branch)])
   (cond
     (~@ NEWLINE clause) ...
     NEWLINE [inner-condition NEWLINE inner-then-branch]
     NEWLINE [else NEWLINE else-branch])])


(define-refactoring-rule cond-begin-to-cond
  #:description "The bodies of cond clauses are already implicitly wrapped in begin."
  #:literals (cond begin)
  [(cond clause-before ...
         [condition (begin body ...)]
         clause-after ...)
   (cond
     (~@ NEWLINE clause-before) ...
     NEWLINE [condition (~@ NEWLINE body) ...]
     (~@ NEWLINE clause-after) ...)])


(define-refactoring-rule or-cond-to-cond
  #:description "This or expression can be turned into a clause of the inner cond expression,\
 reducing nesting."
  #:literals (or cond)
  [(or condition (cond clause ...))
   (cond
     NEWLINE [condition #t]
     (~@ NEWLINE clause) ...)])


(define-refactoring-rule or-or-to-or
  #:description "Nested or expressions are equivalent to a single or expression."
  #:literals (or)
  [(or first-clause clause ... (or inner-clause ...))
   (or first-clause
       (~@ NEWLINE clause) ...
       (~@ NEWLINE inner-clause) ...)])


(define-refactoring-rule and-and-to-and
  #:description "Nested and expressions are equivalent to a single and expression."
  #:literals (and)
  [(and first-clause clause ... (and inner-clause ...))
   (and first-clause
        (~@ NEWLINE clause) ...
        (~@ NEWLINE inner-clause) ...)])


(define-refactoring-rule and-match-to-match
  #:description "This and expression can be turned into a clause of the inner match expression,\
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
   #:rules (list and-and-to-and
                 and-match-to-match
                 cond-begin-to-cond
                 cond-else-if-to-cond
                 define-case-lambda-to-define
                 define-lambda-to-define
                 if-then-begin-to-cond
                 if-else-begin-to-cond
                 if-else-cond-to-cond
                 if-else-if-to-cond
                 if-x-else-x-to-and
                 or-cond-to-cond
                 or-or-to-or)))
