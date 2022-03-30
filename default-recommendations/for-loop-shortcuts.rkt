#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [for-loop-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/set
         rebellion/private/static-name
         resyntax/refactoring-rule
         resyntax/refactoring-suite
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/let-binding
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/syntax-replacement
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class sequence-syntax-convertible-list-expression
  #:attributes (refactored)
  #:literals (vector->list range hash-keys hash-values hash->list bytes->list string->list)

  (pattern (vector->list vec)
    #:attr refactored #'(in-vector vec))

  (pattern (range arg ...)
    #:attr refactored #'(in-range arg ...))

  (pattern (hash-keys hash)
    #:attr refactored #'(in-hash-keys _))

  (pattern (hash-values hash)
    #:attr refactored #'(in-hash-values hash))

  (pattern (hash->list hash)
    #:attr refactored #'(in-hash-pairs hash))

  (pattern (bytes->list bstr)
    #:attr refactored #'(in-bytes bstr))

  (pattern (string->list bstr)
    #:attr refactored #'(in-string bstr))

  (pattern plain-list:expr
    #:attr refactored #'(in-list plain-list)))


(define-syntax-class for-clause-convertible-list-expression
  #:attributes (flat? [leading-clause 1] trailing-expression)

  (pattern
      (append-map
       (_:lambda-by-any-name (y:id) append-map-body:sequence-syntax-convertible-list-expression)
       list-expression:sequence-syntax-convertible-list-expression)
    #:attr flat? #false
    #:with (leading-clause ...) #'([y list-expression.refactored])
    #:with trailing-expression #'append-map-body.refactored)

  (pattern list-expression:sequence-syntax-convertible-list-expression
    #:attr flat? #true
    #:with (leading-clause ...) #'()
    #:with trailing-expression #'list-expression.refactored))


(define-syntax-class for-loop-convertible-list-expression
  #:attributes (loop nesting-loop? loop-clauses [loop-body 1])
  #:literals (map filter append-map)

  (pattern
      (map
       (_:lambda-by-any-name (x:id) loop-body:expr ...+)
       (filter
        (_:lambda-by-any-name (y:id) filter-body:expr)
        list-expression:sequence-syntax-convertible-list-expression))
    #:when (bound-identifier=? #'x #'y)
    #:attr nesting-loop? #false
    #:with loop-clauses #'([x list-expression.refactored] NEWLINE #:when filter-body)
    #:with loop #'(for/list loop-clauses loop-body ...))

  (pattern
      (map
       (_:lambda-by-any-name (x:id) loop-body:expr ...+)
       (append-map
        (_:lambda-by-any-name (y:id) append-map-body:sequence-syntax-convertible-list-expression)
        list-expression:sequence-syntax-convertible-list-expression))
    #:when (not (bound-identifier=? #'x #'y))
    #:attr nesting-loop? #true
    #:with loop-clauses #'([y list-expression.refactored] NEWLINE [x append-map-body.refactored])
    #:with loop #'(for*/list loop-clauses loop-body ...))

  (pattern
      (map
       (_:lambda-by-any-name (x:id) loop-body:expr ...+)
       list-expression:sequence-syntax-convertible-list-expression)
    #:attr nesting-loop? #false
    #:with loop-clauses #'([x list-expression.refactored])
    #:with loop #'(for/list loop-clauses loop-body ...)))


(define-refactoring-rule apply-plus-to-for/sum
  #:description "Applying + to a list of numbers can be replaced with a for/sum loop."
  #:literals (apply +)
  [(apply + loop:for-loop-convertible-list-expression)
   #:with loop-type (if (attribute loop.nesting-loop?) #'for*/sum #'for/sum)
   (loop-type loop.loop-clauses (~@ NEWLINE loop.loop-body) ...)])


(define-syntax-class worthwhile-for-each-function
  #:attributes (x [body 1])

  ;; We always migrate for-each functions that use let expressions, since in the process of migrating
  ;; we can replace the let bindings with internal definitions within the for loop body.
  (pattern (_:lambda-by-any-name (x) original-body:body-with-refactorable-let-expression)
    #:with (body ...) #'(original-body.refactored ...))

  ;; Lambdas with multiple body forms are hard to read when all the forms are on one line, so we
  ;; assume all such lambdas are multi-line, and multi-line for-each functions are typically easier
  ;; to read when they're in the body of a for loop.
  (pattern (_:lambda-by-any-name (x) first-body remaining-body ...+)
    #:with (body ...) #'(NEWLINE (ORIGINAL-SPLICE first-body remaining-body ...)))

  ;; We don't bother migrating for-each forms with only a single body form unless the body form is
  ;; exceptionally long, so that forms which span multiple lines tend to get migrated. By not
  ;; migrating short forms, we avoid bothering reviewers with changes to loops that aren't complex
  ;; enough to need a lot of refactoring in the first place.
  (pattern (_:lambda-by-any-name (x) only-body)
    #:when (>= (syntax-span #'only-body) 60)
    #:with (body ...) #'(NEWLINE only-body)))


(define-refactoring-rule for-each-to-for
  #:description "This for-each operation can be replaced with a for loop."
  #:literals (for-each)
  [(for-each function:worthwhile-for-each-function loop:for-clause-convertible-list-expression)
   #:with loop-type (if (attribute loop.flat?) #'for #'for*)
   (loop-type ((~@ loop.leading-clause NEWLINE) ... [function.x loop.trailing-expression])
              function.body ...)])


(define-refactoring-rule list->vector-for/list-to-for/vector
  #:description "For loops can build vectors directly."
  #:literals (list->vector for/list for*/list)
  [(list->vector
    ((~or (~and for/list (~bind [loop #'for/vector])) (~and for*/list (~bind [loop #'for*/vector])))
     clauses ...))
   (loop (ORIGINAL-SPLICE clauses ...))])


(define-refactoring-rule for/fold-building-hash-to-for/hash
  #:description "This for loop is building a hash and can be simplified."
  #:literals (for/fold for*/fold hash make-immutable-hash)
  [((~or (~and for/fold (~bind [loop #'for/hash])) (~and for*/fold (~bind [loop #'for*/hash])))
    ([h:id (~or (hash) (make-immutable-hash))]) iteration-clauses
     body ...
     (hash-set h-usage:id key value))
   #:when (free-identifier=? #'h #'h-usage)
   #:when (not (set-member? (syntax-free-identifiers #'(body ...)) #'h))
   (loop (ORIGINAL-SPLICE iteration-clauses body ...) NEWLINE (values key value))])


(define for-loop-shortcuts
  (refactoring-suite
   #:name (name for-loop-shortcuts)
   #:rules
   (list apply-plus-to-for/sum
         for/fold-building-hash-to-for/hash
         for-each-to-for
         list->vector-for/list-to-for/vector)))
