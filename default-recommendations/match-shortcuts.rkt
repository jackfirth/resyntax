#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [match-shortcuts refactoring-suite?]))


(require racket/list
         racket/match
         racket/set
         racket/stream
         resyntax/base
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/private/syntax-traversal
         resyntax/private/logger
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-syntax-class single-clause-match
  #:literals (match)
  #:attributes (match-pattern [as-definition-context-body 1])

  (pattern (match subject [match-pattern body ...])
    #:with definition #'(match-define match-pattern subject)
    #:with (as-definition-context-body ...)
    #`(~splicing-replacement (definition body ...) #:original #,this-syntax)))


(define-definition-context-refactoring-rule single-clause-match-to-match-define
  #:description "This `match` expression can be simplified using `match-define`."
  #:literals (match)
  (~seq body-before ... match-expression:single-clause-match)
  #:when (set-empty? (set-intersect (syntax-bound-identifiers #'(body-before ...))
                                    (syntax-bound-identifiers #'match-expression.match-pattern)))
  #:with (new-body ...) (if (empty? (attribute body-before))
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
  #:datum-literals (=>)

  (match val-expr
    clause-before ...
    (~and [match-pattern:expr (~and option-or-body (~not (=> :id))) ...] clause-to-replace)
    clause-after ...)

  #:do [(define match-pattern-with-depths
          (match-pattern-add-ellipsis-depth-properties (attribute match-pattern)))
        (define search-results
          (syntax-search match-pattern-with-depths
            #:datum-literals (?)
            [(? (:lambda-by-any-name (subject-in-condition:id) condition:expr) subject:id)
             #:when (equal? (syntax-property (attribute subject) 'match-pattern-ellipsis-depth) 0)
             (stream
              (unnecessary-predicate-pattern
               (attribute subject) (attribute condition) (attribute subject-in-condition)))]))]
  #:when (equal? (stream-length search-results) 1)
  #:do [(match-define (unnecessary-predicate-pattern subject condition subject-in-condition)
          (stream-first search-results))]

  #:with new-pattern
  (syntax-traverse match-pattern-with-depths
    #:datum-literals (?)
    [(? _ subject2:id)
     #:when (free-identifier=? subject (attribute subject2))
     subject])

  #:with new-condition
  (syntax-traverse condition
    [id:id
     #:when (free-identifier=? #'id subject-in-condition)
     subject])

  #:with new-clause #'[new-pattern #:when new-condition option-or-body ...]

  (match val-expr
    clause-before ...
    (~replacement new-clause #:original clause-to-replace)
    clause-after ...))


(define-refactoring-suite match-shortcuts
   #:rules (predicate-pattern-with-lambda-to-when
            single-clause-match-to-match-define))
