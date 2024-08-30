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
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/let-binding
         resyntax/default-recommendations/private/metafunction
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/default-recommendations/private/syntax-lines
         resyntax/private/syntax-neighbors
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
    #:with flat? #false
    #:with (leading-clause ...) #'([y list-expression.refactored])
    #:with trailing-expression #'append-map-body.refactored)

  (pattern list-expression:sequence-syntax-convertible-list-expression
    #:with flat? #true
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
    #:with nesting-loop? #false
    #:with loop-clauses #'([x list-expression.refactored] #:when filter-body)
    #:with loop #'(for/list loop-clauses loop-body ...))

  (pattern
    (map
     (_:lambda-by-any-name (x:id) loop-body:expr ...+)
     (append-map
      (_:lambda-by-any-name (y:id) append-map-body:sequence-syntax-convertible-list-expression)
      list-expression:sequence-syntax-convertible-list-expression))
    #:when (not (bound-identifier=? #'x #'y))
    #:with nesting-loop? #true
    #:with loop-clauses #'([y list-expression.refactored] [x append-map-body.refactored])
    #:with loop #'(for*/list loop-clauses loop-body ...))

  (pattern
    (map
     (_:lambda-by-any-name (x:id) loop-body:expr ...+)
     list-expression:sequence-syntax-convertible-list-expression)
    #:with nesting-loop? #false
    #:with loop-clauses #'([x list-expression.refactored])
    #:with loop #'(for/list loop-clauses loop-body ...)))


(define-refactoring-rule apply-plus-to-for/sum
  #:description "Applying `+` to a list of numbers can be replaced with a `for/sum` loop."
  #:literals (apply +)
  [(apply + loop:for-loop-convertible-list-expression)
   ((~if loop.nesting-loop? for*/sum for/sum) loop.loop-clauses loop.loop-body ...)])


;; A loop body function is a lambda expression that is passed to a function like map, for-each, or
;; ormap which calls the lambda once for each element of a list. When code is migrated to use for
;; loops, the loop body function becomes the body of the for loop, hence the name.
(define-syntax-class worthwhile-loop-body-function
  #:attributes (x [body 1])

  ;; We always migrate loop functions that use let expressions, since in the process of migrating
  ;; we can replace the let bindings with internal definitions within the for loop body.
  (pattern (_:lambda-by-any-name (x) original-body:body-with-refactorable-let-expression)
    #:with (body ...) #'(original-body.refactored ...))

  ;; Lambdas with multiple body forms are hard to read when all the forms are on one line, so we
  ;; assume all such lambdas are multi-line, and multi-line for-each functions are typically easier
  ;; to read when they're in the body of a for loop.
  (pattern (_:lambda-by-any-name (x) first-body remaining-body ...+)
    #:with (body ...) #'(first-body remaining-body ...))

  ;; We don't bother migrating for-each forms with only a single body form unless the body form is
  ;; exceptionally long, so that forms which span multiple lines tend to get migrated. By not
  ;; migrating short forms, we avoid bothering reviewers with changes to loops that aren't complex
  ;; enough to need a lot of refactoring in the first place.
  (pattern (_:lambda-by-any-name (x) only-body)
    #:when (>= (syntax-span #'only-body) 60)
    #:with (body ...) #'(only-body)))


(define-refactoring-rule for-each-to-for
  #:description "This `for-each` operation can be replaced with a `for` loop."
  #:literals (for-each)
  [(for-each function:worthwhile-loop-body-function loop:for-clause-convertible-list-expression)
   ((~if loop.flat? for for*)
    (loop.leading-clause ... [function.x loop.trailing-expression])
    function.body ...)])


(define-refactoring-rule ormap-to-for/or
  #:description "This `ormap` operation can be replaced with a `for/or` loop."
  #:literals (ormap)
  [(ormap function:worthwhile-loop-body-function loop:for-clause-convertible-list-expression)
   ((~if loop.flat? for/or for*/or)
    (loop.leading-clause ... [function.x loop.trailing-expression])
    function.body ...)])


(define-refactoring-rule andmap-to-for/and
  #:description "This `andmap` operation can be replaced with a `for/and` loop."
  #:literals (andmap)
  [(andmap function:worthwhile-loop-body-function loop:for-clause-convertible-list-expression)
   ((~if loop.flat? for/and for*/and)
    (loop.leading-clause ... [function.x loop.trailing-expression])
    function.body ...)])


(define-syntax-class for-list-id
  #:attributes (set-id vector-id)
  #:literals (for/list for*/list)
  (pattern for/list #:with set-id #'for/set #:with vector-id #'for/vector)
  (pattern for*/list #:with set-id #'for*/set #:with vector-id #'for*/vector))


(define-refactoring-rule list->vector-to-for/vector
  #:description "`for` loops can build vectors directly."
  #:literals (list->vector)
  [(list->vector (loop-id:for-list-id clauses body ...))
   ((~replacement loop-id.vector-id #:original loop-id) clauses body ...)])


(define-refactoring-rule list->set-to-for/set
  #:description "`for` loops can build sets directly"
  #:literals (list->set)
  [(list->set (loop-id:for-list-id clauses body ...))
   ((~replacement loop-id.set-id #:original loop-id) clauses body ...)])


(define-refactoring-rule for/fold-building-hash-to-for/hash
  #:description "This `for` loop is building a hash and can be simplified."
  #:literals (for/fold for*/fold hash make-immutable-hash)
  [((~or (~and for/fold (~bind [loop #'for/hash])) (~and for*/fold (~bind [loop #'for*/hash])))
    ([h:id (~or (hash) (make-immutable-hash))]) iteration-clauses
    body ...
    (hash-set h-usage:id key value))
   #:when (free-identifier=? #'h #'h-usage)
   #:when (not (set-member? (syntax-free-identifiers #'(body ...)) #'h))
   (loop iteration-clauses body ... (values key value))])


(define-syntax-class nested-for

  #:attributes ([clause 1] [body 1])
  #:literals (for)

  (pattern (for (outer-clause) nested:nested-for)
    #:with (clause ...) #'(outer-clause nested.clause ...)
    #:with (body ...) #'(nested.body ...))
  
  (pattern (for (only-clause) body ...)
    #:with (clause ...) #'(only-clause)))


(define-refactoring-rule nested-for-to-for*
  #:description "These nested `for` loops can be replaced by a single `for*` loop."
  [nested:nested-for
   #:when (>= (length (attribute nested.clause)) 2)
   (for* (nested.clause ...)
     nested.body ...)])


(define-refactoring-rule named-let-loop-to-for/first-in-vector
  #:description "This loop can be replaced by a simpler, equivalent `for/first` loop."
  #:literals (let add1 + vector-length vector-ref if and <)
  [(let loop1:id ([i1:id 0])
     (and (< i2:id (vector-length vec1:id))
          (let ([x:id (vector-ref vec2:id i3:id)])
            (if condition:expr
                true-branch:expr
                (loop2:id (~or (add1 i4:id) (+ i4:id 1) (+ 1 i4:id)))))))
   #:when (and (free-identifier=? #'loop1 #'loop2)
               (free-identifier=? #'i1 #'i2)
               (free-identifier=? #'i1 #'i3)
               (free-identifier=? #'i1 #'i4)
               (free-identifier=? #'vec1 #'vec2))
   (for/first ([x (in-vector vec1)] #:when condition)
     true-branch)])


(define-refactoring-rule or-in-for/and-to-filter-clause
  #:description "The `or` expression in this `for` loop can be replaced by a filtering clause."
  #:literals (for/and for*/and or)
  [((~and loop-id (~or for/and for*/and))
    (~and original-clauses (clause ...))
    (~and original-body (or condition:condition-expression ...+ last-condition)))
   (loop-id
    (~replacement
     (clause ... (~@ (~if condition.negated? #:when #:unless) condition.base-condition) ...)
     #:original original-clauses)
    (~replacement last-condition #:original original-body))])


(define-syntax-class apply-append-refactorable-for-loop
  #:attributes (refactored-loop)
  #:literals (for/list for*/list)

  (pattern (for/list (only-clause) only-body:expr)
    #:when (oneline-syntax? #'only-body)
    #:with refactored-loop
    #'(for*/list (only-clause [v (in-list only-body)])
        v))

  (pattern ((~and loop-id for*/list) (clause ...) only-body:expr)
    #:when (oneline-syntax? #'only-body)
    #:with refactored-loop
    #'(loop-id (clause ... [v (in-list only-body)]) v)))


(define-refactoring-rule apply-append-for-loop-to-for-loop
  #:description "Instead of using `(apply append ...)` to flatten a list of lists, consider using\
 `for*/list` to flatten the list."
  #:literals (apply append)
  [(apply append loop:apply-append-refactorable-for-loop)
   loop.refactored-loop])


(define-refactoring-rule when-expression-in-for-loop-to-when-keyword
  #:description "Use the `#:when` keyword instead of `when` to reduce loop body indentation."
  #:literals (when for for*)
  [((~or for-id:for for-id:for*) (clause ...) (when condition body ...))
   (for-id (clause ... #:when condition) body ...)])


(define-refactoring-rule unless-expression-in-for-loop-to-unless-keyword
  #:description "Use the `#:unless` keyword instead of `unless` to reduce loop body indentation."
  #:literals (unless for for*)
  [((~or for-id:for for-id:for*) (clause ...) (unless condition body ...))
   (for-id (clause ... #:unless condition) body ...)])


(define for-loop-shortcuts
  (refactoring-suite
   #:name (name for-loop-shortcuts)
   #:rules
   (list andmap-to-for/and
         apply-append-for-loop-to-for-loop
         apply-plus-to-for/sum
         for/fold-building-hash-to-for/hash
         for-each-to-for
         list->set-to-for/set
         list->vector-to-for/vector
         named-let-loop-to-for/first-in-vector
         nested-for-to-for*
         or-in-for/and-to-filter-clause
         ormap-to-for/or
         unless-expression-in-for-loop-to-unless-keyword
         when-expression-in-for-loop-to-when-keyword)))
