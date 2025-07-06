#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [for-loop-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/set
         resyntax/base
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/let-binding
         resyntax/default-recommendations/private/list-function
         resyntax/default-recommendations/private/metafunction
         resyntax/default-recommendations/private/syntax-equivalence
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/default-recommendations/private/syntax-lines
         resyntax/private/identifier-naming
         resyntax/private/logger
         resyntax/private/syntax-traversal
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
  (apply + loop:for-loop-convertible-list-expression)
  ((~if loop.nesting-loop? for*/sum for/sum) loop.loop-clauses loop.loop-body ...))


;; A loop body function is a lambda expression that is passed to a function like map, for-each, or
;; ormap which calls the lambda once for each element of a list. When code is migrated to use for
;; loops, the loop body function becomes the body of the for loop, hence the name. For convenience,
;; we also accept lambdas which take two arguments such as those used with hash-for-each. Techncially,
;; such a two-argument lambda shouldn't be accepted when in the context of a function like for-each
;; instead of hash-for-each, but we don't bother checking for that since if the code already compiles
;; and runs without any tests failing it probably doesn't have that issue.
(define-syntax-class worthwhile-loop-body-function
  #:attributes (x y [body 1])

  ;; We always migrate loop functions that use let expressions, since in the process of migrating
  ;; we can replace the let bindings with internal definitions within the for loop body.
  (pattern
    (_:lambda-by-any-name (x (~optional (~seq y)))
                          original-body:body-with-refactorable-let-expression)
    #:with (body ...) #'(original-body.refactored ...))

  ;; Lambdas with multiple body forms are hard to read when all the forms are on one line, so we
  ;; assume all such lambdas are multi-line, and multi-line for-each functions are typically easier
  ;; to read when they're in the body of a for loop.
  (pattern (_:lambda-by-any-name (x (~optional (~seq y))) first-body remaining-body ...+)
    #:with (body ...) #'(first-body remaining-body ...))

  ;; We don't bother migrating for-each forms with only a single body form unless the body form is
  ;; exceptionally long, so that forms which span multiple lines tend to get migrated. By not
  ;; migrating short forms, we avoid bothering reviewers with changes to loops that aren't complex
  ;; enough to need a lot of refactoring in the first place.
  (pattern (_:lambda-by-any-name (x (~optional (~seq y))) only-body)
    #:when (>= (syntax-span #'only-body) 60)
    #:with (body ...) #'(only-body)))


(define-refactoring-rule map-to-for
  #:description "This `map` operation can be replaced with a `for/list` loop."
  #:literals (map)
  (map function:worthwhile-loop-body-function loop:for-clause-convertible-list-expression)
  ((~if loop.flat? for/list for*/list)
   (loop.leading-clause ... [function.x loop.trailing-expression])
   function.body ...))


(define-refactoring-rule for-each-to-for
  #:description "This `for-each` operation can be replaced with a `for` loop."
  #:literals (for-each)
  (for-each function:worthwhile-loop-body-function loop:for-clause-convertible-list-expression)
  ((~if loop.flat? for for*)
   (loop.leading-clause ... [function.x loop.trailing-expression])
   function.body ...))


(define-refactoring-rule hash-for-each-to-for
  #:description "This `hash-for-each` operation can be replaced with a `for` loop."
  #:literals (hash-for-each)
  (hash-for-each h function:worthwhile-loop-body-function)
  (for ([(function.x function.y) (in-hash h)])
    function.body ...))


(define-refactoring-rule build-list-to-for
  #:description "This `build-list` operation can be replaced with a `for/list` loop."
  #:literals (build-list)
  (build-list n function:worthwhile-loop-body-function)
  (for/list ([function.x (in-range n)])
    function.body ...))


(define-syntax-class for-loop-supporting-leading-nested-clause
  #:literals (for/list for*/list)
  #:attributes ([clause 1] [body 1])
  (pattern (for/list (only-clause) body ...) #:with (clause ...) (list #'only-clause))
  (pattern (for*/list (clause ...) body ...)))


(define-refactoring-rule append-map-for/list-to-for*/list
  #:description "This `append-map` operation can be replaced with a `for*/list` loop."
  #:literals (append-map)
  (append-map (:lambda-by-any-name (sublist-id:id) loop:for-loop-supporting-leading-nested-clause)
              lists)
  (for*/list ([sublist-id (in-list lists)] loop.clause ...) loop.body ...))


(define-refactoring-rule ormap-to-for/or
  #:description "This `ormap` operation can be replaced with a `for/or` loop."
  #:literals (ormap)
  (ormap function:worthwhile-loop-body-function loop:for-clause-convertible-list-expression)
  ((~if loop.flat? for/or for*/or)
   (loop.leading-clause ... [function.x loop.trailing-expression])
   function.body ...))


(define-refactoring-rule andmap-to-for/and
  #:description "This `andmap` operation can be replaced with a `for/and` loop."
  #:literals (andmap)
  (andmap function:worthwhile-loop-body-function loop:for-clause-convertible-list-expression)
  ((~if loop.flat? for/and for*/and)
   (loop.leading-clause ... [function.x loop.trailing-expression])
   function.body ...))


(define-syntax-class nested-for/or
  #:attributes ([clause 1] [body 1])
  #:literals (for/or)

  (pattern (for/or (outer-clause ...) nested:nested-for/or ~!)
    #:attr [clause 1] (append (attribute outer-clause) (attribute nested.clause))
    #:attr [body 1] (attribute nested.body))

  (pattern (for/or (clause ...) body ...)))


(define-syntax-class nested-for/and
  #:attributes ([clause 1] [body 1])
  #:literals (for/and)

  (pattern (for/and (outer-clause ...) nested:nested-for/and ~!)
    #:attr [clause 1] (append (attribute outer-clause) (attribute nested.clause))
    #:attr [body 1] (attribute nested.body))

  (pattern (for/and (clause ...) body ...)))


(define-refactoring-rule nested-for/or-to-for*/or
  #:description "Nested `for/or` loops can be replaced with a single `for*/loop`."
  #:literals (for/or)
  (for-id:for/or (clause ...) nested:nested-for/or)
  ((~replacement for*/or #:original for-id) (clause ... nested.clause ...) nested.body ...))


(define-refactoring-rule nested-for/and-to-for*/and
  #:description "Nested `for/or` loops can be replaced with a single `for*/loop`."
  #:literals (for/and)
  (for-id:for/and (clause ...) nested:nested-for/and)
  ((~replacement for*/and #:original for-id) (clause ... nested.clause ...) nested.body ...))


(define-syntax-class for-list-id
  #:attributes (set-id vector-id)
  #:literals (for/list for*/list)
  (pattern for/list #:with set-id #'for/set #:with vector-id #'for/vector)
  (pattern for*/list #:with set-id #'for*/set #:with vector-id #'for*/vector))


(define-refactoring-rule list->vector-to-for/vector
  #:description "`for` loops can build vectors directly."
  #:literals (list->vector)
  (list->vector (loop-id:for-list-id clauses body ...))
  ((~replacement loop-id.vector-id #:original loop-id) clauses body ...))


(define-refactoring-rule list->set-to-for/set
  #:description "`for` loops can build sets directly"
  #:literals (list->set)
  (list->set (loop-id:for-list-id clauses body ...))
  ((~replacement loop-id.set-id #:original loop-id) clauses body ...))


(define-definition-context-refactoring-rule for-set!-to-for/fold
  #:description "`for/fold` can be used instead of a mutating `for` loop"
  #:literals (for set! define)
  (~seq body-before ...
        (define accum:id init-expr:expr)
        (for clauses for-body ... (set! accum2:id update-expr:expr))
        body-after ...)
  #:when (free-identifier=? (attribute accum) (attribute accum2))
  (body-before ...
   (define accum
     (for/fold ([accum init-expr])
               clauses
       for-body ...
       update-expr))
   body-after ...))


(define-refactoring-rule for/fold-building-hash-to-for/hash
  #:description "This `for` loop is building a hash and can be simplified."
  #:literals (for/fold for*/fold hash make-immutable-hash hash-set)
  ((~or (~and for/fold (~bind [loop #'for/hash]))
        (~and for*/fold (~bind [loop #'for*/hash])))
   ([h:id (~or (hash) (make-immutable-hash))]) iteration-clauses
   body ...
   (hash-set h-usage:id key value))

  ;; The expansion of for/fold is very complex, and one thing it does is mess with the accumulator ids
  ;; and their uses such that free-identifier=? on an accumulator's use and its binder breaks. To work
  ;; around this, we compare the hash usage and hash accumulator ids by symbol here.
  #:when (equal? (syntax-e #'h) (syntax-e #'h-usage))

  #:do [(define body-ids (syntax-free-identifiers #'(body ...)))]
  #:when (and (not (set-member? body-ids #'h))
              (not (set-member? body-ids #'h-usage)))
  (loop iteration-clauses body ... (values key value)))


(define-definition-context-refactoring-rule for/fold-result-keyword
  #:description
  "Only one of the `for/fold` expression's result values is used. Use the `#:result` keyword to \
return just that result."
  #:literals (define-values for/fold for*/fold)
  (~seq body-before ...
        (~and original-definition
              (define-values (result-id:id ...)
                ((~or for-id:for/fold for-id:for*/fold)
                 ([accumulator-id:id initializer:expr] ...)
                 loop-clauses loop-body ...)))
        body-after ...)
  #:do [(define used-ids
          (for/list ([id (in-list (attribute result-id))]
                     #:when (set-member? (syntax-free-identifiers #'(body-after ...)) id))
            id))]
  #:when (equal? (length used-ids) 1)
  #:cut
  #:do [(define used-index (index-of (attribute result-id) (first used-ids)))
        (define used-accumulator (list-ref (attribute accumulator-id) used-index))]
  #:with replacement-definition
  #`(define #,(first used-ids)
      (for-id ([accumulator-id initializer] ...
               #:result #,used-accumulator)
              loop-clauses loop-body ...))
  (body-before ...
   (~replacement replacement-definition #:original original-definition)
   body-after ...))


(define-refactoring-rule for/fold-with-conditional-body-to-when-keyword
  #:description "This `for/fold` loop can be simplified by using the `#:when` keyword."
  #:literals (for/fold for*/fold)
  ((~or for-id:for/fold for-id:for*/fold)
   (~and orig-accumulators ([accumulator-id:id _]))
   (loop-clause ...)
   conditional-body:if-like-expression)
  #:with (accumulator-id-use:id) #'(conditional-body.false-body ...)
  ;; The expansion of for/fold is very complex, and one thing it does is mess with the accumulator ids
  ;; and their uses such that free-identifier=? on an accumulator's use and its binder breaks. To work
  ;; around this, we compare the usage and accumulator ids by symbol here.
  #:when (equal? (syntax-e (attribute accumulator-id))
                 (syntax-e (attribute accumulator-id-use)))
  (for-id orig-accumulators
          (loop-clause ... #:when conditional-body.base-condition)
          conditional-body.true-body ...))


(define-refactoring-rule for/fold-with-conditional-body-to-unless-keyword
  #:description "This `for/fold` loop can be simplified by using the `#:unless` keyword."
  #:literals (for/fold for*/fold)
  ((~or for-id:for/fold for-id:for*/fold)
   (~and orig-accumulators ([accumulator-id:id _]))
   (loop-clause ...)
   conditional-body:if-like-expression)
  #:with (accumulator-id-use:id) #'(conditional-body.true-body ...)
  ;; The expansion of for/fold is very complex, and one thing it does is mess with the accumulator ids
  ;; and their uses such that free-identifier=? on an accumulator's use and its binder breaks. To work
  ;; around this, we compare the usage and accumulator ids by symbol here.
  #:when (equal? (syntax-e (attribute accumulator-id))
                 (syntax-e (attribute accumulator-id-use)))
  (for-id orig-accumulators
          (loop-clause ... #:unless conditional-body.base-condition)
          conditional-body.false-body ...))


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
  nested:nested-for
  #:when (>= (length (attribute nested.clause)) 2)
  (for* (nested.clause ...)
    nested.body ...))


(define-refactoring-rule named-let-loop-to-for/list
  #:description "This named `let` expression is equivalent to a `for/list` loop."
  #:literals (let cond else null? empty? null quote car first cdr rest cons)
  (let loop:id ([vs:id init-list])
    (cond
      [(:empty-predicate-by-any-name vs2:id) :empty-list-by-any-name]
      [else
       loop-body:expr ...
       (cons loop-element:expr (loop2:id (:rest-by-any-name vs3:id)))]))
  #:when (free-identifier=? #'loop #'loop2)
  #:when (free-identifier=? #'vs #'vs2)
  #:when (free-identifier=? #'vs #'vs3)
  #:when (not
          (for/or ([body-stx (in-list (cons #'loop-element (attribute loop-body)))])
            (syntax-find-first body-stx
              (~and (~var usage (expression-directly-enclosing (attribute vs)))
                    (~not (:first-by-any-name _))))))
  #:cut

  #:with element-id (depluralize-id #'vs)

  #:with (modified-result-element modified-body ...)
  (for/list ([body-stx (cons #'loop-element (attribute loop-body))])
    (syntax-traverse body-stx
      [(:first-by-any-name vs-usage:id) #:when (free-identifier=? #'vs-usage #'vs) #'element-id]))

  (for/list ([element-id (in-list init-list)])
    modified-body ...
    modified-result-element))


(define-refactoring-rule named-let-loop-to-for/and
  #:description "This named `let` expression is equivalent to a `for/and` loop."
  #:literals (let cond else)
  (let loop:id ([vs:id init-list])
    (cond
      [(:empty-predicate-by-any-name vs2:id) #true]
      [element-condition:expr (loop2:id (:rest-by-any-name vs3:id))]
      [else #false]))

  #:when (free-identifier=? (attribute loop) (attribute loop2))
  #:when (free-identifier=? (attribute vs) (attribute vs2))
  #:when (free-identifier=? (attribute vs) (attribute vs3))
  #:when (not (syntax-find-first (attribute element-condition)
                (~and (~var usage (expression-directly-enclosing (attribute vs)))
                      (~not (:first-by-any-name _)))))
  #:cut

  #:with element-id (depluralize-id (attribute vs))
  #:with modified-element-condition
  (syntax-traverse (attribute element-condition)
    [(:first-by-any-name vs-usage:id)
     #:when (free-identifier=? (attribute vs) (attribute vs-usage))
     (attribute element-id)])

  (for/and ([element-id (in-list init-list)])
    modified-element-condition))


(define-refactoring-rule named-let-loop-to-for/or
  #:description "This named `let` expression is equivalent to a `for/or` loop."
  #:literals (let cond else)
  (let loop:id ([vs:id init-list])
    (cond
      [(:empty-predicate-by-any-name vs2:id) #false]
      [element-condition:expr #true]
      [else (loop2:id (:rest-by-any-name vs3:id))]))

  #:when (free-identifier=? (attribute loop) (attribute loop2))
  #:when (free-identifier=? (attribute vs) (attribute vs2))
  #:when (free-identifier=? (attribute vs) (attribute vs3))
  #:when (not (syntax-find-first (attribute element-condition)
                (~and (~var usage (expression-directly-enclosing (attribute vs)))
                      (~not (:first-by-any-name _)))))
  #:cut

  #:with element-id (depluralize-id (attribute vs))
  #:with modified-element-condition
  (syntax-traverse (attribute element-condition)
    [(:first-by-any-name vs-usage:id)
     #:when (free-identifier=? (attribute vs) (attribute vs-usage))
     (attribute element-id)])

  (for/or ([element-id (in-list init-list)])
    modified-element-condition))
  


(define-refactoring-rule named-let-loop-to-for/first-in-vector
  #:description "This loop can be replaced by a simpler, equivalent `for/first` loop."
  #:literals (let add1 + vector-length vector-ref if and <)
  (let loop1:id ([i1:id 0])
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
    true-branch))


(define-refactoring-rule or-let-in-for/and-to-filter-clause
  #:description
  "The `or` expression in this `for` loop can be replaced by a filtering clause, letting you use\
 `define` instead of `let` in the loop body."
  #:literals (for/and for*/and or)
  ((~and loop-id (~or for/and for*/and))
   (~and original-clauses (clause ...))
   (~and original-body
         (or condition:condition-expression ...+ last-condition:refactorable-let-expression)))
  (loop-id
   (~replacement
    (clause ... (~@ (~if condition.negated? #:when #:unless) condition.base-condition) ...)
    #:original original-clauses)
   (~@ . (~splicing-replacement (last-condition.refactored ...) #:original original-body))))


(define-refactoring-rule when-expression-in-for-loop-to-when-keyword
  #:description "Use the `#:when` keyword instead of `when` to reduce loop body indentation."
  #:literals (when for for*)
  ((~or for-id:for for-id:for*) (clause ...) (when condition body ...))
  (for-id (clause ... #:when condition) body ...))


(define-refactoring-rule unless-expression-in-for-loop-to-unless-keyword
  #:description "Use the `#:unless` keyword instead of `unless` to reduce loop body indentation."
  #:literals (unless for for*)
  ((~or for-id:for for-id:for*) (clause ...) (unless condition body ...))
  (for-id (clause ... #:unless condition) body ...))


(define-refactoring-suite for-loop-shortcuts
  #:rules (andmap-to-for/and
           append-map-for/list-to-for*/list
           apply-plus-to-for/sum
           build-list-to-for
           for/fold-building-hash-to-for/hash
           for/fold-result-keyword
           for/fold-with-conditional-body-to-unless-keyword
           for/fold-with-conditional-body-to-when-keyword
           for-each-to-for
           for-set!-to-for/fold
           hash-for-each-to-for
           list->set-to-for/set
           list->vector-to-for/vector
           map-to-for
           named-let-loop-to-for/and
           named-let-loop-to-for/first-in-vector
           named-let-loop-to-for/list
           named-let-loop-to-for/or
           nested-for-to-for*
           nested-for/and-to-for*/and
           nested-for/or-to-for*/or
           or-let-in-for/and-to-filter-clause
           ormap-to-for/or
           unless-expression-in-for-loop-to-unless-keyword
           when-expression-in-for-loop-to-when-keyword))
