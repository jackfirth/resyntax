#lang racket/base


(require racket/contract)


(provide
 (contract-out
  [refactoring-rule? predicate/c]
  [refactoring-rule-description (-> refactoring-rule? immutable-string?)]
  [standard-refactoring-rules (listof refactoring-rule?)]))


(module+ private
  (provide
   (contract-out
    [refactoring-rule-refactor (-> refactoring-rule? syntax? (option/c syntax-replacement?))])))


(require (for-syntax racket/base)
         (only-in racket/class
                  define/augment
                  define/augment-final
                  define/augride
                  define/overment
                  define/override
                  define/override-final
                  define/public
                  define/public-final
                  define/pubment
                  define/private)
         racket/list
         racket/match
         racket/sequence
         racket/syntax
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/private/guarded-block
         rebellion/type/object
         resyntax/let-binding
         resyntax/syntax-replacement
         syntax/parse
         syntax/parse/define
         syntax/parse/lib/function-header)


(module+ test
  (require (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-object-type refactoring-rule (transformer description)
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
  (define-refactoring-rule id:id
    #:description description
    parse-option ...
    [pattern pattern-directive ... replacement])
  #:declare description (expr/c #'string?)
  (define id
    (constructor:refactoring-rule
     #:name 'id
     #:description (string->immutable-string description.c)
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
  #:description "The define-struct form exists for backwards compatibility, struct is preferred."
  #:literals (define-struct)
  [(define-struct id-maybe-super:define-struct-id-maybe-super fields
     (~and option (~not #:constructor-name) (~not #:extra-constructor-name)) ...)
   #:with make-id (format-id #'id-maybe-super.id "make-~a" #'id-maybe-super.id)
   (struct id-maybe-super.id (~? id-maybe-super.super-id) fields NEWLINE
     #:extra-constructor-name make-id NEWLINE
     option ...)])


(define-refactoring-rule false/c-migration
  #:description "false/c is an alias for #f that exists for backwards compatibility."
  #:literals (false/c)
  [false/c
   #false])


(define-refactoring-rule symbols-migration
  #:description "symbols is equivalent to or/c and exists for backwards compatibility."
  #:literals (symbols)
  [(symbols sym ...)
   (or/c sym ...)])


(define-refactoring-rule vector-immutableof-migration
  #:description "vector-immutableof is a legacy form that is equivalent to vectorof with the\
 #:immutable option"
  #:literals (vector-immutableof)
  [(vector-immutableof c)
   (vectorof c #:immutable #true)])


(define-refactoring-rule vector-immutable/c-migration
  #:description "vector-immutable/c is a legacy form that is equivalent to vector/c with the\
 #:immutable option"
  #:literals (vector-immutable/c)
  [(vector-immutable/c c ...)
   (vector/c c ... #:immutable #true)])


(define-refactoring-rule box-immutable/c-migration
  #:description "box-immutable/c is a legacy form that is equivalent to box/c with the #:immutable\
 option"
  #:literals (box-immutable/c)
  [(box-immutable/c c)
   (box/c c #:immutable #true)])


(define-refactoring-rule flat-contract-migration
  #:description "flat-contract is a legacy form for constructing contracts from predicates;\
 predicates can be used directly as contracts now."
  #:literals (flat-contract)
  [(flat-contract predicate)
   predicate])


(define-refactoring-rule flat-contract-predicate-migration
  #:description "flat-contract is a legacy form for turning contracts into predicates; flat contracts\
 can be used directly as predicates now."
  #:literals (flat-contract-predicate)
  [(flat-contract-predicate c)
   c])


(define-refactoring-rule contract-struct-migration
  #:description "The contract-struct form is deprecated, use struct instead. Lazy struct contracts no\
 longer require a separate struct declaration."
  #:literals (contract-struct)
  [(contract-struct id fields)
   (struct id fields)])


(define-refactoring-rule define-contract-struct-migration
  #:description "The define-contract-struct form is deprecated, use struct instead. Lazy struct\
 contracts no longer require a separate struct declaration."
  #:literals (define-contract-struct)
  [(define-contract-struct id fields)
   #:with make-id (format-id #'id "make-~a" #'id)
   (struct id fields #:extra-constructor-name make-id)])


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


;@----------------------------------------------------------------------------------------------------
;; DEFINITION CONTEXT RULES


(define-refactoring-rule let-to-define
  #:description "Internal definitions are recommended instead of let expressions, to reduce nesting."
  [(header:header-form-allowing-internal-definitions let-expr:body-with-refactorable-let-expression)
   (header.formatted ... let-expr.refactored ...)])


(define-splicing-syntax-class header-form-allowing-internal-definitions
  #:attributes ([formatted 1])
  #:literals (let let* let-values when unless with-handlers parameterize)

  (pattern (~seq lambda:lambda-by-any-name ~! formals:formals)
    #:with (formatted ...) #'(lambda formals))

  (pattern (~seq define:define-by-any-name ~! header:function-header)
    #:with (formatted ...) #'(define header))

  (pattern (~seq let ~! (~optional name:id) header)
    #:with (formatted ...) #'(let (~? name) header))

  (pattern (~seq let* ~! header)
    #:with (formatted ...) #'(let* header))

  (pattern (~seq let-values ~! header)
    #:with (formatted ...) #'(let-values header))

  (pattern (seq when ~! condition)
    #:with (formatted ...) #'(when condition))

  (pattern (seq unless ~! condition)
    #:with (formatted ...) #'(unless condition))

  (pattern (seq with-handlers ~! handlers)
    #:with (formatted ...) #'(with-handlers handlers))

  (pattern (seq parameterize ~! handlers)
    #:with (formatted ...) #'(parameterize handlers)))


;; λ and lambda aren't free-identifier=?. Additionally, by using a syntax class instead of #:literals
;; we can produce the same lambda identifier that the input syntax had instead of changing all lambda
;; identfiers to one of the two cases. There doesn't seem to be a strong community consensus on which
;; name should be used, so we want to avoid changing the original code's choice.
(define-syntax-class lambda-by-any-name
  #:literals (λ lambda)
  (pattern (~or λ lambda)))


;; There's a lot of variants of define that support the same grammar but have different meanings. We
;; can recognize and refactor all of them with this syntax class.
(define-syntax-class define-by-any-name
  #:literals (define
               define/augment
               define/augment-final
               define/augride
               define/overment
               define/override
               define/override-final
               define/public
               define/public-final
               define/pubment
               define/private)
  (pattern
      (~or define
           define/augment
           define/augment-final
           define/augride
           define/overment
           define/override
           define/override-final
           define/public
           define/public-final
           define/pubment
           define/private)))


(define-refactoring-rule and-let-to-cond-define
  #:description "This and expression can be turned into a cond expression with internal definitions,\
 reducing nesting."
  #:literals (and let)
  [(and guard-expr let-expr:refactorable-let-expression)
   (cond
     NEWLINE [(not guard-expr) #false]
     NEWLINE [else let-expr.refactored ...])])


(define-syntax-class cond-clause
  #:attributes ([formatted 1])
  #:literals (else =>)
  (pattern (~and clause (~or [else body ...+] [expr:expr => body-handler:expr] [expr:expr body ...+]))
    #:with (formatted ...)
    #'(NEWLINE clause)))


(define-syntax-class refactorable-cond-clause
  #:attributes ([refactored 1])
  #:literals (else =>)

  (pattern [else let-expr:body-with-refactorable-let-expression]
    #:with (refactored ...) #'(NEWLINE [else let-expr.refactored ...]))
  
  (pattern (~and [expr let-expr:body-with-refactorable-let-expression] (~not [expr => _ ...]))
    #:with (refactored ...) #'(NEWLINE [expr let-expr.refactored ...])))


(define-refactoring-rule cond-let-to-cond-define
  #:description "The body of a cond clause supports internal definitions, which are preferred over\
 let expressions because they reduce nesting."
  #:literals (cond)
  [(cond
     clause-before:cond-clause ...
     refactorable:refactorable-cond-clause
     clause-after:cond-clause ...)
   (cond
     clause-before.formatted ... ...
     refactorable.refactored ...
     clause-after.formatted ... ...)])


(define-refactoring-rule if-then-let-to-cond-define
  #:description "This if expression can be turned into a cond expression with internal definitions,\
 reducing nesting."
  #:literals (if)
  [(if condition let-expr:refactorable-let-expression else-expr)
   (cond
     NEWLINE [condition let-expr.refactored ...]
     NEWLINE [else NEWLINE else-expr])])


(define-refactoring-rule if-else-let-to-cond-define
  #:description "This if expression can be turned into a cond expression with internal definitions,\
 reducing nesting."
  #:literals (if)
  [(if condition then-expr let-expr:refactorable-let-expression)
   (cond
     NEWLINE [condition NEWLINE then-expr]
     NEWLINE [else let-expr.refactored ...])])


(define-refactoring-rule let*-once-to-let
  #:description "A let* expression with a single binding is equivalent to a let expression."
  #:literals (let*)
  [(let* (~and header ([id:id rhs:expr])) body ...)
   (let header (~@ NEWLINE body) ...)])


(define definition-context-refactoring-rules
  (list
   let-to-define
   and-let-to-cond-define
   cond-let-to-cond-define
   if-then-let-to-cond-define
   if-else-let-to-cond-define
   let*-once-to-let))


;@----------------------------------------------------------------------------------------------------
;; FOR LOOP REFACTORING RULES


(define-syntax-class for-clause-convertible-list-expression
  #:attributes (flat? [leading-clause 1] trailing-expression)

  (pattern (append-map (_:lambda-by-any-name (y:id) append-map-body:expr) list-expression:expr)
    #:attr flat? #false
    #:with (leading-clause ...) #'([y (in-list list-expression)])
    #:with trailing-expression #'(in-list append-map-body))

  (pattern list-expression:expr
    #:attr flat? #true
    #:with (leading-clause ...) #'()
    #:with trailing-expression #'(in-list list-expression)))


(define-syntax-class for-loop-convertible-list-expression
  #:attributes (loop nesting-loop? loop-clauses [loop-body 1])
  #:literals (map filter append-map)

  (pattern
      (map
       (_:lambda-by-any-name (x:id) loop-body:expr ...+)
       (filter (_:lambda-by-any-name (y:id) filter-body:expr) list-expression))
    #:when (bound-identifier=? #'x #'y)
    #:attr nesting-loop? #false
    #:with loop-clauses #'([x (in-list list-expression)] NEWLINE #:when filter-body)
    #:with loop #'(for/list loop-clauses loop-body ...))

  (pattern
      (map
       (_:lambda-by-any-name (x:id) loop-body:expr ...+)
       (append-map (_:lambda-by-any-name (y:id) append-map-body:expr) list-expression))
    #:when (not (bound-identifier=? #'x #'y))
    #:attr nesting-loop? #true
    #:with loop-clauses #'([y (in-list list-expression)] NEWLINE [x (in-list append-map-body)])
    #:with loop #'(for*/list loop-clauses loop-body ...))

  (pattern (map (_:lambda-by-any-name (x:id) loop-body:expr ...+) list-expression)
    #:attr nesting-loop? #false
    #:with loop-clauses #'([x (in-list list-expression)])
    #:with loop #'(for/list loop-clauses loop-body ...)))


(define-refactoring-rule apply-plus-to-for/sum
  #:description "Applying + to a list of numbers can be replaced with a for/sum loop."
  #:literals (apply +)
  [(apply + loop:for-loop-convertible-list-expression)
   #:with loop-type (if (attribute loop.nesting-loop?) #'for*/sum #'for/sum)
   (loop-type loop.loop-clauses (~@ NEWLINE loop.loop-body) ...)])


(define-refactoring-rule ormap-to-for/or
  #:description "This ormap operation can be replaced with a for/or loop."
  #:literals (ormap)
  [(ormap (_:lambda-by-any-name (x:id) body:expr ...+) loop:for-clause-convertible-list-expression)
   #:with loop-type (if (attribute loop.flat?) #'for/or #'for*/or)
   (loop-type ((~@ loop.leading-clause NEWLINE) ... [x loop.trailing-expression])
              (~@ NEWLINE body) ...)])


(define-refactoring-rule andmap-to-for/and
  #:description "This andmap operation can be replaced with a for/and loop."
  #:literals (andmap)
  [(andmap (_:lambda-by-any-name (x:id) body:expr ...+) loop:for-clause-convertible-list-expression)
   #:with loop-type (if (attribute loop.flat?) #'for/and #'for*/and)
   (loop-type ((~@ loop.leading-clause NEWLINE) ... [x loop.trailing-expression])
              (~@ NEWLINE body) ...)])


(define-refactoring-rule in-syntax-list-to-in-syntax
  #:description
  "The in-syntax function can be used instead of converting this syntax object to a list."
  #:literals (in-list syntax->list)
  [(in-list (syntax->list stx))
   (in-syntax stx)])


(define-refactoring-rule for/or-and-to-for/first
  #:description "This for/or loop can be replaced with a simpler, equivalent for/first loop."
  #:literals (for/or and)
  [(for/or (clause ...) (and (pred:id x:id) y:id))
   #:when (free-identifier=? #'x #'y)
   (for/first (clause ... #:when (pred x)) x)])


(define for-loop-refactoring-rules
  (list
   andmap-to-for/and
   apply-plus-to-for/sum
   for/or-and-to-for/first
   in-syntax-list-to-in-syntax
   ormap-to-for/or))


;@----------------------------------------------------------------------------------------------------
;; STANDARD RULE LIST


(define standard-refactoring-rules
  (append definition-context-refactoring-rules
          for-loop-refactoring-rules
          (list and-and-to-and
                and-match-to-match
                box-immutable/c-migration
                cond-begin-to-cond
                cond-else-if-to-cond
                contract-struct-migration
                define-case-lambda-to-define
                define-contract-struct-migration
                define-lambda-to-define
                false/c-migration
                flat-contract-migration
                flat-contract-predicate-migration
                if-then-begin-to-cond
                if-else-begin-to-cond
                if-else-cond-to-cond
                if-else-if-to-cond
                if-x-else-x-to-and
                or-cond-to-cond
                or-or-to-or
                struct-from-define-struct-with-default-constructor-name
                symbols-migration
                vector-immutableof-migration
                vector-immutable/c-migration)))
