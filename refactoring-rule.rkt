#lang racket/base

(require racket/contract)

(provide
 (contract-out
  [refactoring-rule? predicate/c]
  [standard-refactoring-rules (listof refactoring-rule?)]))

(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor (-> refactoring-rule? syntax? (option/c syntax-replacement?))])))

(require (for-syntax racket/base)
         fancy-app
         racket/list
         racket/match
         racket/sequence
         racket/set
         racket/syntax
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/private/guarded-block
         rebellion/type/object
         resyntax/source-code
         resyntax/syntax-rendering
         syntax/id-set
         syntax/parse
         syntax/parse/define
         syntax/parse/lib/function-header
         syntax/stx)

(module+ test
  (require (submod "..")
           rackunit))

;@----------------------------------------------------------------------------------------------------


(define-object-type refactoring-rule (transformer)
  #:omit-root-binding
  #:constructor-name constructor:refactoring-rule)


(define (refactoring-rule-refactor rule syntax)
  (define rule-introduction-scope (make-syntax-introducer))
  (option-map
   ((refactoring-rule-transformer rule) (rule-introduction-scope syntax))
   (λ (new-syntax)
     (syntax-replacement
      #:original-syntax syntax #:new-syntax (rule-introduction-scope new-syntax)))))


(define-simple-macro
  (define-refactoring-rule id:id parse-option ... [pattern pattern-directive ... replacement])
  (define id
    (constructor:refactoring-rule
     #:name 'id
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
   (cond
     NEWLINE [condition (~@ NEWLINE then-body) ...]
     NEWLINE [else NEWLINE else-branch])])


(define-refactoring-rule cond-from-if-else-begin
  #:literals (if begin)
  [(if condition then-branch (begin else-body ...))
   (cond
     NEWLINE [condition NEWLINE then-branch]
     NEWLINE [else (~@ NEWLINE else-body) ...])])


(define-refactoring-rule if-then-cond-to-cond
  #:literals (if cond)
  [(if condition then-branch (cond clause ...))
   (cond
     NEWLINE [condition NEWLINE then-branch]
     (~@ NEWLINE clause) ...)])


(define-refactoring-rule match-absorbing-outer-and
  #:literals (and match)
  [(and and-subject:id (match match-subject:id match-clause ...))
   #:when (free-identifier=? #'and-subject #'match-subject)
   (match match-subject
     NEWLINE [#false #false]
     (~@ NEWLINE match-clause) ...)])


;@----------------------------------------------------------------------------------------------------
;; DEFINITION CONTEXT RULES


(define (sequence->bound-id-set ids)
  (immutable-bound-id-set (list->set (sequence->list ids))))


(define/guard (syntax-identifiers stx)
  (guard (identifier? stx) then
    (list stx))
  (guard (stx-list? stx) else
    (list))
  (for*/list ([substx (in-syntax stx)]
              [subid (in-list (syntax-identifiers substx))])
    subid))


(module+ test
  (test-case "syntax-identifiers"
    (check-equal?
     (map syntax->datum (syntax-identifiers #'(hello (darkness #:my old) friend)))
     (list 'hello 'darkness 'old 'friend))))


(define (no-binding-overlap? ids other-ids)
  (define id-set (sequence->bound-id-set ids))
  (define other-id-set (sequence->bound-id-set other-ids))
  (bound-id-set-empty? (bound-id-set-intersect id-set other-id-set)))


(module+ test
  (test-case "no-binding-overlap?"
    (check-true (no-binding-overlap? (in-syntax #'(a b c)) (in-syntax #'(d e f))))
    (check-false (no-binding-overlap? (in-syntax #'(a b c)) (in-syntax #'(c d e))))
    (check-true (no-binding-overlap? (in-syntax #'(a b c)) '()))
    (check-true (no-binding-overlap? '() (in-syntax #'(d e f))))))


(define-syntax-class refactorable-let-bindings
  #:attributes (ids [definition 1] unmigratable-bindings fully-migratable?)
  (pattern ((~and clause [id:id rhs:expr]) ...)
    #:with ids #'(id ...)
    #:do [(define id-vec (for/vector ([id-stx (in-syntax #'ids)]) id-stx))
          (define rhs-vec (for/vector ([rhs-stx (in-syntax #'(rhs ...))]) rhs-stx))
          (define clause-vec (for/vector ([clause-stx (in-syntax #'(clause ...))]) clause-stx))
          (define used-ids (syntax-identifiers #'(rhs ...)))
          (define-values (safe-positions unsafe-positions)
            (for/fold ([safe '()]
                       [unsafe '()]
                       #:result (values (reverse safe) (reverse unsafe)))
                      ([i (in-naturals)]
                       [id-stx (in-vector id-vec)])
              (if (no-binding-overlap? (list id-stx) used-ids)
                  (values (cons i safe) unsafe)
                  (values safe (cons i unsafe)))))]
    #:when (not (empty? safe-positions))
    #:with (definition ...)
    (for/list ([pos (in-list safe-positions)])
      (define id-stx (vector-ref id-vec pos))
      (define rhs-stx (vector-ref rhs-vec pos))
      (define total-length (+ (syntax-span id-stx) (syntax-span rhs-stx)))
      (if (> total-length 90) ;; conservative under-estimate
          #`(define #,id-stx NEWLINE #,rhs-stx)
          #`(define #,id-stx #,rhs-stx)))
    #:with unmigratable-bindings
    (for/list ([pos (in-list unsafe-positions)]
               [n (in-naturals)]
               #:when #true
               [part
                (in-list
                 (if (zero? n)
                     (list (vector-ref clause-vec pos))
                     (list #'NEWLINE (vector-ref clause-vec pos))))])
      part)
    #:attr fully-migratable? (zero? (length unsafe-positions))))


(define-syntax-class refactorable-let-expression
  #:literals (let)
  #:attributes (ids [refactored-form 1])
  (pattern (let bindings:refactorable-let-bindings body ...)
    #:with ids #'bindings.ids
    #:with (refactored-form ...)
    (if (attribute bindings.fully-migratable?)
        #'((~@ NEWLINE bindings.definition) ...
           (~@ NEWLINE body) ...)
        #'((~@ NEWLINE bindings.definition) ...
           NEWLINE (let bindings.unmigratable-bindings
                     (~@ NEWLINE body) ...)))))


(define-refactoring-rule define-let-to-define
  #:literals (define let)
  [(define header:function-header let-expr:refactorable-let-expression)
   #:when (no-binding-overlap? (in-syntax #'header.params) (in-syntax #'let-expr.ids))
   (define header let-expr.refactored-form ...)])


(define-refactoring-rule and-let-to-cond-define
  #:literals (and let)
  [(and guard-expr let-expr:refactorable-let-expression)
   (cond
     NEWLINE [(not guard-expr) #false]
     NEWLINE [else let-expr.refactored-form ...])])


(define-refactoring-rule cond-else-let-to-define
  #:literals (cond else let)
  [(cond
     clause ...
     [else let-expr:refactorable-let-expression])
   (cond
     (~@ NEWLINE clause) ...
     NEWLINE [else let-expr.refactored-form ...])])


(define-refactoring-rule if-then-let-else-let-to-cond-define
  #:literals (if else let)
  [(if condition
       then-let-expr:refactorable-let-expression
       else-let-expr:refactorable-let-expression)
   (cond
     NEWLINE [condition then-let-expr.refactored-form ...]
     NEWLINE [else else-let-expr.refactored-form ...])])


(define-refactoring-rule if-then-let-to-cond-define
  #:literals (if else let)
  [(if condition
       let-expr:refactorable-let-expression
       then-expr)
   (cond
     NEWLINE [condition let-expr.refactored-form ...]
     NEWLINE [else NEWLINE then-expr])])


(define-refactoring-rule if-else-let-to-cond-define
  #:literals (if else let)
  [(if condition
       then-expr
       let-expr:refactorable-let-expression)
   (cond
     NEWLINE [condition NEWLINE then-expr]
     NEWLINE [else let-expr.refactored-form ...])])


;@----------------------------------------------------------------------------------------------------
;; STANDARD RULE LIST


(define standard-refactoring-rules
  (list and-let-to-cond-define
        box-immutable/c-migration
        cond-else-let-to-define
        cond-from-if-then-begin
        cond-from-if-else-begin
        cond-mandatory-else
        contract-struct-migration
        define-contract-struct-migration
        define-from-case-lambda
        define-let-to-define
        false/c-migration
        flat-contract-migration
        flat-contract-predicate-migration
        if-then-cond-to-cond
        if-then-let-else-let-to-cond-define
        if-then-let-to-cond-define
        if-else-let-to-cond-define
        match-absorbing-outer-and
        struct-from-define-struct-with-default-constructor-name
        symbols-migration
        vector-immutableof-migration
        vector-immutable/c-migration))
