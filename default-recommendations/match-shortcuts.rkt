#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [match-shortcuts refactoring-suite?]))


(require racket/list
         racket/match
         racket/set
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/private/syntax-traversal
         syntax/parse
         syntax/strip-context)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-syntax-class single-clause-match
  #:literals (match)
  #:attributes (match-pattern subject [body 1] [as-definition-context-body 1])

  (pattern (match subject [match-pattern body ...])
    #:with definition #'(match-define match-pattern subject)
    #:with (as-definition-context-body ...)
    #`(~splicing-replacement (definition body ...) #:original #,this-syntax)))


(define-definition-context-refactoring-rule single-clause-match-to-match-define
  #:description "This `match` expression can be simplified using `match-define`."
  #:literals (match)
  (~seq body-before ... match-expression:single-clause-match)

  #:do
  [(define pattern-ids
     (syntax-bound-identifiers (attribute match-expression.match-pattern)))
   (define pattern-ids-in-surrounding-context
     (syntax-bound-identifiers
      (replace-context (attribute match-expression) (attribute match-expression.match-pattern))))
   (define body-ids (syntax-bound-identifiers #'(body-before ... match-expression.subject)))
   (define subject-ids-in-body-context
     (syntax-bound-identifiers
      (replace-context
       (first (attribute match-expression.body)) (attribute match-expression.subject))))]
  #:when (set-empty? (set-intersect pattern-ids-in-surrounding-context body-ids))
  #:when (set-empty? (set-intersect pattern-ids subject-ids-in-body-context))
  #:with (new-body ...)
  (if (empty? (attribute body-before))
      (attribute match-expression.as-definition-context-body)
      #'(~focus-replacement-on
         (match-expression.as-definition-context-body ...)))

  (body-before ... new-body ...))


(struct unnecessary-predicate-pattern (subject-id condition-expr subject-in-condition-id)
  #:transparent)


(define-syntax-class match-pattern-ellipsis
  #:literals (...)
  (pattern ...)
  (pattern id:id
    #:do [(define id-str (symbol->string (syntax-e #'id)))]
    #:when (or (equal? id-str "___")
               (regexp-match-exact? #rx"..\\d" id-str)
               (regexp-match-exact? #rx"__\\d" id-str))))


(define (match-pattern-add-ellipsis-depth-properties pattern-stx)
  (let loop ([pattern-stx pattern-stx] [depth 0])
    (syntax-traverse pattern-stx
      [((~seq (~and subpattern-before-ellipsis (~not :match-pattern-ellipsis)) ...
              (~and subpattern (~not :match-pattern-ellipsis))
              ellipsis:match-pattern-ellipsis ...+)
        ...+
        (~and subpattern-after-ellipsis (~not :match-pattern-ellipsis)) ...)
       #:with ((subpattern-before-ellipsis-with-prop ...) ...)
       (for/list ([subpattern-before-ellipsis-lists (in-list (attribute subpattern-before-ellipsis))])
         (for/list ([subpattern-before-ellipsis-stx (in-list subpattern-before-ellipsis-lists)])
           (loop subpattern-before-ellipsis-stx depth)))
       #:with (subpattern-with-prop ...)
       (for/list ([subpattern-stx (in-list (attribute subpattern))]
                  [ellipsis-list (in-list (attribute ellipsis))])
         (define new-depth (+ depth (length ellipsis-list)))
         (loop subpattern-stx new-depth))
       #:with (subpattern-after-ellipsis-with-prop ...)
       (for/list ([subpattern-after-ellipsis-stx (in-list (attribute subpattern-after-ellipsis))])
         (loop subpattern-after-ellipsis-stx depth))
       #'((~@ subpattern-before-ellipsis-with-prop ... subpattern-with-prop ellipsis ...) ...
          subpattern-after-ellipsis-with-prop ...)]
      [(~and id:id (~not :match-pattern-ellipsis))
       (syntax-property (attribute id) 'match-pattern-ellipsis-depth depth)])))


(module+ test
  (test-case "match-pattern-add-ellipsis-depth-properties"
    (define (report-depths stx)
      (syntax->datum
       (syntax-traverse (match-pattern-add-ellipsis-depth-properties stx)
         [id:id
          (datum->syntax #false (syntax-property (attribute id) 'match-pattern-ellipsis-depth))])))
    (check-equal? (report-depths #'(list x y z)) '(0 0 0 0))
    (check-equal? (report-depths #'(list (list x) (list y) (list z))) '(0 (0 0) (0 0) (0 0)))
    (check-equal? (report-depths #'(list xs (... ...))) '(0 1 #false))
    (check-equal? (report-depths #'(list xs (... ...) y z)) '(0 1 #false 0 0))
    (check-equal? (report-depths #'(list (list xs ys zs) (... ...))) '(0 (1 1 1 1) #false))))


(define-refactoring-rule predicate-pattern-with-lambda-to-when
  #:description "This `match` pattern using `?` with a lambda can be simplified using `#:when`."
  #:literals (match)
  #:datum-literals (? =>)

  (match val-expr
    clause-before ...
    (~and [match-pattern:expr (~and option-or-body (~not (=> :id))) ...] clause-to-replace)
    clause-after ...)

  #:with (? (_ (subject-in-condition) condition) subject)
  (syntax-find-first (match-pattern-add-ellipsis-depth-properties (attribute match-pattern))
    #:datum-literals (?)
    (? (:lambda-by-any-name (subject-in-condition:id) condition:expr) subject:id)
    #:when (equal? (syntax-property (attribute subject) 'match-pattern-ellipsis-depth) 0))

  #:with new-pattern
  (syntax-traverse (attribute match-pattern)
    #:datum-literals (?)
    [(? _ subject2:id)
     #:when (free-identifier=? (attribute subject) (attribute subject2))
     (attribute subject)])

  #:with new-condition
  (syntax-traverse (attribute condition)
    [id:id
     #:when (free-identifier=? (attribute id) (attribute subject-in-condition))
     (attribute subject)])

  #:with new-clause #'[new-pattern #:when new-condition option-or-body ...]

  (match val-expr
    clause-before ...
    (~replacement new-clause #:original clause-to-replace)
    clause-after ...))


(define-syntax-class conditional-body
  #:description "conditional expression in match clause body"
  #:attributes (condition then-expr else-expr)
  #:literals (if cond else)
  
  (pattern (if condition then-expr else-expr))
  
  (pattern (cond [condition then-expr] [else else-expr])))


(define-refactoring-rule match-conditional-to-when
  #:description "This conditional in a `match` clause can be simplified using `#:when`."
  #:literals (match)

  (match subject
    clause-before ...
    (~and [pattern conditional:conditional-body] clause-to-replace)
    clause-after ...+)

  (match subject
    clause-before ...
    [pattern #:when conditional.condition conditional.then-expr]
    [pattern conditional.else-expr]
    clause-after ...))


(define-refactoring-suite match-shortcuts
  #:rules (match-conditional-to-when
           predicate-pattern-with-lambda-to-when
           single-clause-match-to-match-define))
