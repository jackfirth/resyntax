#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [for-loop-shortcuts refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/sequence
         racket/set
         resyntax/base
         resyntax/default-recommendations/analyzers/identifier-usage
         resyntax/default-recommendations/loops/private/syntax-classes
         resyntax/default-recommendations/private/boolean
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/let-replacement/private/let-binding
         resyntax/default-recommendations/private/list-function
         resyntax/default-recommendations/private/literal-constant
         resyntax/default-recommendations/private/metafunction
         resyntax/default-recommendations/private/syntax-equivalence
         resyntax/default-recommendations/private/syntax-identifier-sets
         resyntax/default-recommendations/private/syntax-lines
         resyntax/private/identifier-naming
         resyntax/private/logger
         resyntax/private/syntax-traversal
         syntax/parse
         syntax/parse/define)


;@----------------------------------------------------------------------------------------------------


(define-literal-set simple-for-loops
  (for
      for*
    for/list
    for*/list
    for/vector
    for*/vector
    for/set
    for*/set 
    for/sum
    for*/sum
    for/product
    for*/product
    for/and
    for*/and
    for/or
    for*/or
    for/first
    for*/first
    for/last
    for*/last
    for/hash
    for*/hash))


(define-refactoring-rule hash-for-each-to-for
  #:description "This `hash-for-each` operation can be replaced with a `for` loop."
  #:literals (hash-for-each)
  (hash-for-each h function:worthwhile-loop-body-function)
  (for ([(function.x function.y) (in-hash h)])
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


(define-refactoring-rule for/vector-with-in-range-to-length
  #:description
  "Add `#:length` to `for/vector` loops to improve performance when the number of iterations is \
known."
  #:literals (for/vector in-range)
  (for/vector
   (~and for-clauses ([var:id (in-range (~optional 0) end:id)]))
   body:expr ...+)
  (for/vector #:length end for-clauses body ...))


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
   (~focus-replacement-on (~replacement replacement-definition #:original original-definition))
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


(define-refactoring-rule or-let-in-for/and-to-filter-clause
  #:description
  "The `or` expression in this `for` loop can be replaced by a filtering clause, letting you use\
 `define` instead of `let` in the loop body."
  #:analyzers (list identifier-usage-analyzer)
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


(define-refactoring-rule in-hash-to-in-hash-keys
  #:description "This `in-hash` can be replaced with `in-hash-keys` since the value is not used."
  #:analyzers (list identifier-usage-analyzer)
  #:literals (in-hash)
  (for-id:id (clause-before ... [(key:id value:id) (in-hash hash-expr)] clause-after ...) body ...)
  #:when ((literal-set->predicate simple-for-loops) (attribute for-id))
  #:when (equal? (syntax-property #'value 'usage-count) 0)
  (for-id (clause-before ... [key (in-hash-keys hash-expr)] clause-after ...) body ...))


(define-refactoring-rule in-hash-to-in-hash-values
  #:description "This `in-hash` can be replaced with `in-hash-values` since the key is not used."
  #:literals (in-hash)
  (for-id:id (clause-before ... [(key:id value:id) (in-hash hash-expr)] clause-after ...) body ...)
  #:when ((literal-set->predicate simple-for-loops) (attribute for-id))
  #:when (equal? (syntax-property #'key 'usage-count) 0)
  (for-id (clause-before ... [value (in-hash-values hash-expr)] clause-after ...) body ...))


(define-refactoring-rule in-value-to-do
  #:description
  "This `in-value` clause can be replaced with a `#:do` clause since the variable is not used."
  #:literals (in-value)
  (for-id:id (clause-before ... [var:id (in-value expr)] clause-after ...) body ...)
  #:when ((literal-set->predicate simple-for-loops) (attribute for-id))
  #:when (equal? (syntax-property #'var 'usage-count) 0)
  (for-id (clause-before ... #:do [expr] clause-after ...) body ...))


(define-refactoring-rule sequence-tail-in-vector-to-in-vector
  #:description "The `in-vector` function accepts an optional start index, making `sequence-tail` unnecessary."
  #:literals (sequence-tail in-vector)
  (sequence-tail (in-vector vec) start)
  (in-vector vec start))


(define-refactoring-suite for-loop-shortcuts
  #:rules (for/fold-building-hash-to-for/hash
           for/fold-result-keyword
           for/fold-with-conditional-body-to-unless-keyword
           for/fold-with-conditional-body-to-when-keyword
           for/vector-with-in-range-to-length
           for-set!-to-for/fold
           hash-for-each-to-for
           in-hash-to-in-hash-keys
           in-hash-to-in-hash-values
           in-value-to-do
           nested-for-to-for*
           nested-for/and-to-for*/and
           nested-for/or-to-for*/or
           or-let-in-for/and-to-filter-clause
           sequence-tail-in-vector-to-in-vector
           unless-expression-in-for-loop-to-unless-keyword
           when-expression-in-for-loop-to-when-keyword))
