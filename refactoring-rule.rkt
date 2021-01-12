#lang racket/base

(require racket/contract)

(provide
 NEWLINE
 (contract-out
  [refactoring-rule? predicate/c]
  [standard-refactoring-rules (listof refactoring-rule?)]))

(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor (-> refactoring-rule? syntax? (option/c syntax?))])))

(require (for-syntax racket/base)
         racket/match
         racket/syntax
         rebellion/base/option
         rebellion/private/guarded-block
         rebellion/type/record
         syntax/parse
         syntax/parse/define)

;@----------------------------------------------------------------------------------------------------

(define-syntax (NEWLINE stx)
  (raise-syntax-error
   'NEWLINE
   "should only be used by refactoring rules to indicate where newlines should be inserted"
   stx))

(define-record-type refactoring-rule (transformer)
  #:omit-root-binding)

(define (refactoring-rule-refactor rule syntax)
  ((refactoring-rule-transformer rule) syntax))

(define-simple-macro
  (define-refactoring-rule id:id parse-option ... [pattern pattern-directive ... replacement])
  (define id
    (constructor:refactoring-rule
     #:transformer
     (syntax-parser
       parse-option ...
       [pattern pattern-directive ... (present #'replacement)]
       [_ absent]))))

(define-syntax-class define-struct-id-maybe-super
  #:attributes (id super-id)
  (pattern id:id #:attr super-id #false)
  (pattern (id:id super-id:id)))

(define-refactoring-rule struct-from-define-struct-with-default-constructor-name
  #:literals (define-struct)
  [(define-struct id-maybe-super:define-struct-id-maybe-super fields
     (~and option (~not #:constructor-name) (~not #:extra-constructor-name)) ...)
   #:with make-id (format-id #'id-maybe-super.id "make-~a" #'id-maybe-super.id)
   (struct id-maybe-super.id (~? id-maybe-super.super-id) fields NEWLINE
     #:extra-constructor-name make-id NEWLINE
     option ...)])

(define-refactoring-rule false/c-migration
  #:literals (false/c)
  [false/c
   #false])

(define-refactoring-rule symbols-migration
  #:literals (symbols)
  [(symbols sym ...)
   (or/c sym ...)])

(define-refactoring-rule vector-immutableof-migration
  #:literals (vector-immutableof)
  [(vector-immutableof c)
   (vectorof c #:immutable #true)])

(define-refactoring-rule vector-immutable/c-migration
  #:literals (vector-immutable/c)
  [(vector-immutable/c c ...)
   (vector/c c ... #:immutable #true)])

(define-refactoring-rule box-immutable/c-migration
  #:literals (box-immutable/c)
  [(box-immutable/c c)
   (box/c c #:immutable #true)])

(define-refactoring-rule flat-contract-migration
  #:literals (flat-contract)
  [(flat-contract predicate)
   predicate])

(define-refactoring-rule flat-contract-predicate-migration
  #:literals (flat-contract-predicate)
  [(flat-contract-predicate c)
   c])

(define-refactoring-rule contract-struct-migration
  #:literals (contract-struct)
  [(contract-struct id fields)
   (struct id fields)])

(define-refactoring-rule define-contract-struct-migration
  #:literals (define-contract-struct)
  [(define-contract-struct id fields)
   #:with make-id (format-id #'id "make-~a" #'id)
   (struct id fields #:extra-constructor-name make-id)])

(define-syntax-class cond-condition-clause
  #:literals (cond else)
  (pattern [(~and condition (~not else)) body ...+]))

(define-refactoring-rule cond-mandatory-else
  #:literals (cond else)
  [(cond clause:cond-condition-clause ...)
   (cond (~@ NEWLINE clause) ... NEWLINE [else (void)])])

(define/guard (free-identifiers=? ids other-ids)
  (define id-list (syntax->list ids))
  (define other-id-list (syntax->list other-ids))
  (guard (equal? (length id-list) (length other-id-list)) else
    #false)
  (for/and ([id (in-list id-list)] [other-id (in-list other-id-list)])
    (free-identifier=? id other-id)))

(define-refactoring-rule define-from-case-lambda
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

(define-refactoring-rule cond-from-if-then-begin
  #:literals (if begin)
  [(if condition (begin then-body ...) else-branch)
   (cond [condition (~@ NEWLINE then-body) ...] NEWLINE
         [else else-branch])])

(define-refactoring-rule cond-from-if-else-begin
  #:literals (if begin)
  [(if condition then-branch (begin else-body ...))
   (cond [condition then-branch] NEWLINE
         [else (~@ NEWLINE else-body) ...])])

(define-refactoring-rule match-absorbing-outer-and
  #:literals (and match)
  [(and and-subject:id (match match-subject:id match-clause ...))
   #:when (free-identifier=? #'and-subject #'match-subject)
   (match match-subject
     NEWLINE [#false #false]
     (~@ NEWLINE match-clause) ...)])

(define standard-refactoring-rules
  (list struct-from-define-struct-with-default-constructor-name
        false/c-migration
        symbols-migration
        vector-immutableof-migration
        vector-immutable/c-migration
        box-immutable/c-migration
        flat-contract-migration
        flat-contract-predicate-migration
        contract-struct-migration
        define-contract-struct-migration
        define-from-case-lambda
        cond-from-if-then-begin
        cond-from-if-else-begin
        match-absorbing-outer-and
        cond-mandatory-else))
