#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [for-loop-shortcuts (listof refactoring-rule?)]))


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
         resyntax/default-recommendations/private/let-binding
         resyntax/refactoring-rule
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/syntax-replacement
         syntax/parse
         syntax/parse/define
         syntax/parse/lib/function-header)


;@----------------------------------------------------------------------------------------------------


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


(define for-loop-shortcuts
  (list andmap-to-for/and
        apply-plus-to-for/sum
        for/or-and-to-for/first
        in-syntax-list-to-in-syntax
        ormap-to-for/or))
