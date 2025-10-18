#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [list-loopification refactoring-suite?]))


(require (for-syntax racket/base)
         racket/list
         racket/set
         resyntax/base
         resyntax/default-recommendations/loops/private/syntax-classes
         resyntax/default-recommendations/private/lambda-by-any-name
         resyntax/default-recommendations/private/metafunction
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class for-list-id
  #:attributes (set-id vector-id)
  #:literals (for/list for*/list)
  (pattern for/list #:with set-id #'for/set #:with vector-id #'for/vector)
  (pattern for*/list #:with set-id #'for*/set #:with vector-id #'for*/vector))


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


(define-refactoring-rule build-list-to-for
  #:description "This `build-list` operation can be replaced with a `for/list` loop."
  #:literals (build-list)
  (build-list n function:worthwhile-loop-body-function)
  (for/list ([function.x (in-range n)])
    function.body ...))


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


(define-refactoring-rule index-mutating-map-to-for/list
  #:description
  "Instead of mutating an index inside a `map` expression, you can use `for/list` with `in-naturals`."
  #:literals (let map + add1 set!)
  (let ([i:id n:nat])
    (map (:lambda-by-any-name
          (v:id)
          (set! i2:id (~or (add1 i3:id) (+ i3:id 1) (+ 1 i3:id)))
          body)
         vs:sequence-syntax-convertible-list-expression))
  #:when (free-identifier=? (attribute i) (attribute i2))
  #:when (free-identifier=? (attribute i) (attribute i3))
  #:with starting-index (add1 (syntax-e (attribute n)))
  (for/list ([v vs.refactored]
             [i (in-naturals starting-index)])
    body))


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


(define-refactoring-rule apply-plus-to-for/sum
  #:description "Applying `+` to a list of numbers can be replaced with a `for/sum` loop."
  #:literals (apply +)
  (apply + loop:for-loop-convertible-list-expression)
  ((~if loop.nesting-loop? for*/sum for/sum) loop.loop-clauses loop.loop-body ...))


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


(define-refactoring-suite list-loopification
  #:rules (andmap-to-for/and
           append-map-for/list-to-for*/list
           apply-plus-to-for/sum
           build-list-to-for
           for-each-to-for
           index-mutating-map-to-for/list
           list->set-to-for/set
           list->vector-to-for/vector
           map-to-for
           ormap-to-for/or))
