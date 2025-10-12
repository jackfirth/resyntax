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
         resyntax/default-recommendations/private/syntax-lines
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


(define-refactoring-rule remove-unnecessary-root-and-pattern
  #:description
  "This `match` expression has an unnecessary root-level `and` pattern that can be simplified."
  #:literals (match and)

  (match subject-var:id
    clause-before ...
    [(and bound-id:id inner-pattern) body-part ...]
    clause-after ...)

  #:with (modified-body ...)
  (for/list ([part (in-list (attribute body-part))])
    (syntax-traverse part
      [id:id
       #:when (free-identifier=? (attribute id) (attribute bound-id))
       (attribute subject-var)]))

  (match subject-var
    clause-before ...
    [inner-pattern modified-body ...]
    clause-after ...))


(define-syntax-class conditional-body
  #:description "conditional expression in match clause body"
  #:attributes (condition [then-expr 1] [else-expr 1])
  #:literals (if cond else =>)
  (pattern (if condition only-then-expr only-else-expr)
    #:attr [then-expr 1] (list (attribute only-then-expr))
    #:attr [else-expr 1] (list (attribute only-else-expr)))
  (pattern (cond [condition then-expr ...+] [else else-expr ...])
    #:and (~parse (~not =>) (first (attribute then-expr)))))


(define-refactoring-rule match-conditional-to-when
  #:description "This conditional in a `match` clause can be simplified using `#:when`."
  #:literals (match)

  (match subject
    clause-before ...
    (~and [pattern conditional:conditional-body] clause-to-replace)
    clause-after ...+)

  #:when (positive? (+ (length (attribute clause-before)) (length (attribute clause-after))))

  ; Duplicating a long or complex match pattern isn't worth the reduction in nesting from
  ; refactoring the code to use #:when, so we don't bother if the pattern is multiple lines or if
  ; it's a fairly long line.
  #:when (oneline-syntax? (attribute pattern))
  #:when (<= (syntax-span (attribute pattern)) 60)

  ; Conversely, a conditional with short body expressions isn't worth duplicating the condition
  ; expression, so we only attempt to refactor conditionals whose body expressions aren't short.
  #:when (or (> (length (attribute conditional.then-expr)) 1)
             (> (length (attribute conditional.else-expr)) 1)
             (>= (syntax-span (first (attribute conditional.then-expr))) 60)
             (>= (syntax-span (first (attribute conditional.else-expr))) 60))

  (match subject
    clause-before ...
    (~@ . (~splicing-replacement ([pattern #:when conditional.condition conditional.then-expr ...]
                                  [pattern conditional.else-expr ...])
                                 #:original pattern))
    clause-after ...))


(define-syntax-class list-ref-expr
  #:attributes (list-id index)
  #:literals (list-ref first second third fourth fifth sixth seventh eighth ninth tenth)

  (pattern (list-ref list-id:id index-constant:nat)
    #:attr index (syntax-e (attribute index-constant)))

  (pattern (first list-id:id) #:attr index 0)
  (pattern (second list-id:id) #:attr index 1)
  (pattern (third list-id:id) #:attr index 2)
  (pattern (fourth list-id:id) #:attr index 3)
  (pattern (fifth list-id:id) #:attr index 4)
  (pattern (sixth list-id:id) #:attr index 5)
  (pattern (seventh list-id:id) #:attr index 6)
  (pattern (eighth list-id:id) #:attr index 7)
  (pattern (ninth list-id:id) #:attr index 8)
  (pattern (tenth list-id:id) #:attr index 9))


(define-definition-context-refactoring-rule list-element-definitions-to-match-define
  #:description "These list element variable definitions can be expressed more succinctly with \
`match-define`. Note that the suggested replacement raises an error if the list contains more \
elements than expected."
  #:literals (define)
  (~seq body-before ... (define v:id ref-expr:list-ref-expr) ...+ body-after ...)
  #:do [(define num-vars (length (attribute v)))]
  #:with first-list-id (first (attribute ref-expr.list-id))
  #:when (for/and ([list-id (in-list (rest (attribute ref-expr.list-id)))])
           (free-identifier=? list-id (attribute first-list-id)))
  #:when (equal? (attribute ref-expr.index) (range 0 num-vars))
  #:when (not (syntax-find-first #'(body-before ... body-after ...)
                id:id
                #:when (free-identifier=? (attribute id) (attribute first-list-id))
                #:when (not (equal? (syntax-property (attribute id) 'usage-count) num-vars))))
  (body-before ... (match-define (list v ...) first-list-id) body-after ...))


(define-refactoring-suite match-shortcuts
  #:rules (list-element-definitions-to-match-define
           match-conditional-to-when
           predicate-pattern-with-lambda-to-when
           remove-unnecessary-root-and-pattern
           single-clause-match-to-match-define))
