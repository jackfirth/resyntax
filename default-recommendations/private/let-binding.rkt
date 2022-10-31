#lang racket/base


(provide body-with-refactorable-let-expression
         refactorable-let-expression)


(require racket/list
         racket/match
         racket/sequence
         racket/set
         racket/syntax
         rebellion/base/option
         rebellion/collection/entry
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/default-recommendations/private/graph
         resyntax/private/source
         resyntax/private/syntax-replacement
         syntax/id-set
         syntax/parse
         syntax/parse/lib/function-header
         syntax/stx)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-syntax-class refactorable-let-expression
  #:attributes ([refactored 1])
  #:literals (let let-values let* let*-values)

  (pattern
      ((~or* let let-values) ~! bindings:refactorable-let-bindings body:body-forms)

    #:when (no-binding-overlap?
            (in-syntax #'(body.bound-id ...)) (in-syntax #'(bindings.inner-bound-id ...)))

    #:when (no-binding-conflicts? (attribute bindings.bound-id) #'body.scopes)

    #:when (attribute bindings.fully-refactorable?)
    #:with (refactored ...) #'(bindings.outer-definition ... body.formatted ...))

  (pattern ((~or* let* let*-values) ~! bindings:refactorable-let*-bindings body:body-forms)
      
    #:when (no-binding-overlap?
            (in-syntax #'(body.bound-id ...)) (in-syntax #'(bindings.inner-bound-id ...)))

    #:when (attribute bindings.fully-refactorable?)
    #:with (refactored ...) #'(bindings.outer-definition ... body.formatted ...)))


(define-splicing-syntax-class body-with-refactorable-let-expression
  #:attributes ([refactored 1])
  #:literals (let let-values let* let*-values)

  (pattern
      (~seq
       leading-body:body-forms
       ((~or* let let-values) ~! bindings:refactorable-let-bindings inner-body:body-forms))
    
    #:when (no-binding-overlap? (syntax-identifiers #'leading-body)
                                (in-syntax #'(bindings.outer-bound-id ...)))
    #:when (no-binding-overlap? (in-syntax #'(inner-body.bound-id ...))
                                (in-syntax #'(bindings.inner-bound-id ...)))

    #:when (no-binding-conflicts? (attribute bindings.bound-id) #'inner-body.scopes)

    #:when (attribute bindings.fully-refactorable?)

    #:with (refactored ...)
    #'(leading-body.formatted ...
       bindings.outer-definition ...
       inner-body.formatted ...))

  (pattern
      (~seq
       leading-body:body-forms
       ((~or* let* let*-values) ~! bindings:refactorable-let*-bindings inner-body:body-forms))
    
    #:when (no-binding-overlap? (syntax-identifiers #'leading-body)
                                (in-syntax #'(bindings.outer-bound-id ...)))
    #:when (no-binding-overlap? (in-syntax #'(inner-body.bound-id ...))
                                (in-syntax #'(bindings.inner-bound-id ...)))

    #:when (no-binding-conflicts? (attribute bindings.bound-id) #'inner-body.scopes)

    #:when (attribute bindings.fully-refactorable?)
    
    #:with (refactored ...)
    #'(leading-body.formatted ...
       bindings.outer-definition ...
       inner-body.formatted ...)))


(module+ test
  (test-case (name-string body-with-refactorable-let-expression)

    (define (parse stx)
      (syntax-parse stx
        [(let-expr:body-with-refactorable-let-expression) (syntax->datum #'(let-expr.refactored ...))]
        [_ #false]))

    (test-case "refactorable let bindings after non-conflicting definitions"
      (define stx #'((define a 1) (let ([b a]) (+ a b))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b a)
          NEWLINE
          (+ a b)))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings after non-conflicting definitions"
      (define stx #'((define a 1) (let* ([b a]) (+ a b))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b a)
          NEWLINE
          (+ a b)))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings after conflicting definitions"
      (define stx #'((define a 1) (let ([a 2]) a)))
      (check-false (parse stx)))

    (test-case "refactorable let* bindings after conflicting definitions"
      (define stx #'((define a 1) (let* ([a 2]) a)))
      (check-false (parse stx)))

    (test-case "refactorable let bindings after capturing definitions"
      (define stx #'((define a x) (let ([x 1]) (+ a x))))
      (check-false (parse stx)))

    (test-case "refactorable let* bindings after capturing definitions"
      (define stx #'((define a x) (let* ([x 1]) (+ a x))))
      (check-false (parse stx)))))


(define-syntax-class refactorable-let-bindings
  #:attributes ([bound-id 1]
                [outer-bound-id 1]
                [inner-bound-id 1]
                [outer-definition 1]
                [inner-definition 1]
                fully-refactorable?
                unrefactorable)
  (pattern (clause:binding-clause ...)
    #:with (bound-id ...)
    (append-map parsed-binding-clause-bound-identifiers (attribute clause.parsed))
    #:do
    [(define parsed-clauses (vector->immutable-vector (list->vector (attribute clause.parsed))))
     (define deps (let-binding-clause-dependencies parsed-clauses))
     (define depgraph (edges->graph deps #:vertex-count (vector-length parsed-clauses)))
     (define graph
       (parsed-binding-graph
        #:clauses parsed-clauses
        #:dependencies depgraph))
     (define split (let-binding-graph-split graph))]
    #:when (split-bindings-changed? split)
    #:attr fully-refactorable? (split-bindings-fully-refactorable? split)
    #:with (outer-bound-id ...) (split-bindings-outer-ids split)
    #:with (inner-bound-id ...) (split-bindings-inner-ids split)
    #:with (outer-definition ...) (split-bindings-outer-definitions split)
    #:with (inner-definition ...) (split-bindings-inner-definitions split)
    #:with unrefactorable (split-bindings-unrefactorable split)))


(define-syntax-class refactorable-let*-bindings
  #:attributes ([bound-id 1]
                [outer-bound-id 1]
                [inner-bound-id 1]
                [outer-definition 1]
                [inner-definition 1]
                fully-refactorable?
                unrefactorable)
  (pattern (clause:binding-clause ...)
    #:with (bound-id ...)
    (append-map parsed-binding-clause-bound-identifiers (attribute clause.parsed))
    #:do
    [(define parsed-clauses (vector->immutable-vector (list->vector (attribute clause.parsed))))
     (define deps (let-binding-clause-dependencies parsed-clauses))
     (define depgraph (edges->graph deps #:vertex-count (vector-length parsed-clauses)))
     (define graph
       (parsed-binding-graph
        #:clauses parsed-clauses
        #:dependencies depgraph))
     (define split (let*-binding-graph-split graph))]
    #:when (split-bindings-changed? split)
    #:attr fully-refactorable? (split-bindings-fully-refactorable? split)
    #:with (outer-bound-id ...) (split-bindings-outer-ids split)
    #:with (inner-bound-id ...) (split-bindings-inner-ids split)
    #:with (outer-definition ...) (split-bindings-outer-definitions split)
    #:with (inner-definition ...) (split-bindings-inner-definitions split)
    #:with unrefactorable (split-bindings-unrefactorable split)))


(define (sequence->bound-id-set ids)
  (immutable-bound-id-set (list->set (sequence->list ids))))


(define/guard (syntax-identifiers stx)
  (guard (identifier? stx) then
    (list stx))
  (guard (stx-list? stx) else
    (list))
  (for*/list ([substx (in-syntax stx)]
              [subid (in-list (syntax-identifiers substx))])
    subid))


(module+ test
  (test-case "syntax-identifiers"
    (check-equal?
     (map syntax->datum (syntax-identifiers #'(hello (darkness #:my old) friend)))
     (list 'hello 'darkness 'old 'friend))))


(define (no-binding-overlap? ids other-ids)
  (define id-set (sequence->bound-id-set ids))
  (define other-id-set (sequence->bound-id-set other-ids))
  (bound-id-set-empty? (bound-id-set-intersect id-set other-id-set)))


(module+ test
  (test-case "no-binding-overlap?"
    (check-true (no-binding-overlap? (in-syntax #'(a b c)) (in-syntax #'(d e f))))
    (check-false (no-binding-overlap? (in-syntax #'(a b c)) (in-syntax #'(c d e))))
    (check-true (no-binding-overlap? (in-syntax #'(a b c)) '()))
    (check-true (no-binding-overlap? '() (in-syntax #'(d e f))))))


(define (no-binding-conflicts? ids body-scopes)
  (for/and ([x (in-list ids)])
    (free-identifier=? (or (scopes-by-location x) x)
                       (datum->syntax body-scopes (syntax-e x)))))


(define-record-type parsed-binding-clause
  (original bound-identifiers identifier-side right-hand-side referenced-identifiers))


(define-syntax-class binding-clause
  #:attributes (parsed)
  (pattern (~and original [id:id rhs:expr])
    #:attr parsed
    (parsed-binding-clause
     #:original #'original
     #:bound-identifiers (list #'id)
     #:identifier-side #'id
     #:right-hand-side #'rhs
     #:referenced-identifiers (syntax-identifiers #'rhs)))
  (pattern (~and original [(~and id-side (id:id ...)) rhs:expr])
    #:attr parsed
    (parsed-binding-clause
     #:original #'original
     #:bound-identifiers (syntax->list #'(id ...))
     #:identifier-side #'id-side
     #:right-hand-side #'rhs
     #:referenced-identifiers (syntax-identifiers #'rhs))))


(define (parsed-binding-clause-definition clause)
  (define rhs (parsed-binding-clause-right-hand-side clause))
  (define id-side (parsed-binding-clause-identifier-side clause))
  (define long? (> (+ (syntax-span id-side) (syntax-span rhs)) 90)) ;; conservative under-estimate
  (define different-lines? (not (equal? (syntax-line id-side) (syntax-line rhs))))
  (match (parsed-binding-clause-bound-identifiers clause)
    [(list id)
     (syntax-parse rhs
       #:literals (lambda λ)
       [((~or lambda λ) (arg:formal ...) body ...)
        #`(define (#,id (~@ (~? arg.kw) (~? [arg.name arg.default] arg.name)) ...)
            (~@ NEWLINE body) ...)]
       [((~or lambda λ) (arg:formal ... . rest:identifier) body ...)
        #`(define (#,id (~@ (~? arg.kw) (~? [arg.name arg.default] arg.name)) ... . rest)
            (~@ NEWLINE body) ...)]
       [((~or lambda λ) args:identifier body ...)
        #:with id* id
        #`(define (id* . args)
            (~@ NEWLINE body) ...)]
       [_
        (cond
          [different-lines? #`(define #,id (ORIGINAL-GAP #,id-side #,rhs) #,rhs)]
          [long? #`(define #,id NEWLINE #,rhs)]
          [else #`(define #,id #,rhs)])])]
    [_
     (cond
       [different-lines? #`(define-values (ORIGINAL-SPLICE #,id-side #,rhs))]
       [long? #`(define-values #,id-side NEWLINE #,rhs)]
       [else #`(define-values #,id-side #,rhs)])]))


(define (binding-clause-depends-on? dependant dependency)
  (define dependant-references (parsed-binding-clause-referenced-identifiers dependant))
  (define dependency-ids (parsed-binding-clause-bound-identifiers dependency))
  (not (no-binding-overlap? dependant-references dependency-ids)))


(define-record-type parsed-binding-graph (clauses dependencies))


(define (let-binding-clause-dependencies clauses)
  (for*/list ([(dependant i) (in-indexed (in-vector clauses))]
              [(dependency j) (in-indexed (in-vector clauses))]
              #:when (binding-clause-depends-on? dependant dependency))
    (entry i j)))
    

(define-record-type split-bindings (before-cycles cycles after-cycles changed?))


(define/guard (let-binding-graph-split graph)
  (define binding-count (vector-length (parsed-binding-graph-clauses graph)))
  (define cycle-indices (graph-cycle-vertices (parsed-binding-graph-dependencies graph)))
  (define changed? (not (equal? (length cycle-indices) binding-count)))
  (guard (empty? cycle-indices) then
    (split-bindings
     #:before-cycles (vector->list (parsed-binding-graph-clauses graph))
     #:cycles '()
     #:after-cycles '()
     #:changed? changed?))
  (define cycle-start-index (transduce cycle-indices #:into (nonempty-into-min)))
  (define cycle-end-index (add1 (transduce cycle-indices #:into (nonempty-into-max))))
  (define before-cycles
    (for/list ([i (in-range 0 cycle-start-index)])
      (vector-ref (parsed-binding-graph-clauses graph) i)))
  (define cycles
    (for/list ([i (in-range cycle-start-index cycle-end-index)])
      (vector-ref (parsed-binding-graph-clauses graph) i)))
  (define after-cycles
    (for/list ([i (in-range cycle-end-index binding-count)])
      (vector-ref (parsed-binding-graph-clauses graph) i)))
  (split-bindings
   #:before-cycles before-cycles
   #:cycles cycles
   #:after-cycles after-cycles
   #:changed? changed?))


(define/guard (let*-binding-graph-split graph)
  (define binding-count (vector-length (parsed-binding-graph-clauses graph)))
  (define (referenced-by-earlier? i)
    (define earliest-predecessor
      (transduce (graph-predecessors (parsed-binding-graph-dependencies graph) i) #:into into-first))
    (match earliest-predecessor
      [(present s) (<= s i)]
      [_ #false]))
  (define cycle-start-index-opt
    (transduce (in-range 0 binding-count)
               (filtering referenced-by-earlier?)
               #:into into-first))
  (guard-match (present cycle-start-index) cycle-start-index-opt else
    (split-bindings
     #:before-cycles (vector->list (parsed-binding-graph-clauses graph))
     #:cycles '()
     #:after-cycles '()
     #:changed? (positive? binding-count)))
  (define cycle-end-index
    (add1
     (transduce (in-range (sub1 binding-count) -1 -1)
                (filtering referenced-by-earlier?)
                #:into nonempty-into-first)))
  (define before-cycles
    (for/list ([i (in-range 0 cycle-start-index)])
      (vector-ref (parsed-binding-graph-clauses graph) i)))
  (define cycles
    (for/list ([i (in-range cycle-start-index cycle-end-index)])
      (vector-ref (parsed-binding-graph-clauses graph) i)))
  (define after-cycles
    (for/list ([i (in-range cycle-end-index binding-count)])
      (vector-ref (parsed-binding-graph-clauses graph) i)))
  (split-bindings
   #:before-cycles before-cycles
   #:cycles cycles
   #:after-cycles after-cycles
   #:changed? (or (not (empty? before-cycles)) (not (empty? after-cycles)))))


(define (split-bindings-fully-refactorable? split)
  (and (empty? (split-bindings-cycles split))
       (empty? (split-bindings-after-cycles split))))


(define (split-bindings-outer-ids split)
  (for*/list ([before (in-list (split-bindings-before-cycles split))]
              [id (in-list (parsed-binding-clause-bound-identifiers before))])
    id))


(define (split-bindings-inner-ids split)
  (for*/list ([after (in-list (split-bindings-after-cycles split))]
              [id (in-list (parsed-binding-clause-bound-identifiers after))])
    id))


(define (split-bindings-outer-definitions split)
  (define/with-syntax (definition ...)
    (for/list ([before (in-list (split-bindings-before-cycles split))])
      (parsed-binding-clause-definition before)))
  #'((~@ NEWLINE definition) ...))


(define (split-bindings-inner-definitions split)
  (define/with-syntax (definition ...)
    (for/list ([after (in-list (split-bindings-after-cycles split))])
      (parsed-binding-clause-definition after)))
  #'((~@ NEWLINE definition) ...))


(define (split-bindings-unrefactorable split)
  #`(#,@(add-between
         (for/list ([clause (in-list (split-bindings-cycles split))])
           (parsed-binding-clause-original clause))
         #'NEWLINE)))


(define-syntax-class body-form
  #:literals (define define-syntax define-values define-syntaxes)
  #:attributes ([bound-id 1])
  (pattern (define id:id ~! _) #:with (bound-id ...) #'(id))
  (pattern (define header:function-header ~! _ ...) #:with (bound-id ...) #'(header.name))
  (pattern (define-syntax id:id ~! _) #:with (bound-id ...) #'(id))
  (pattern (define-syntax header:function-header ~! _ ...) #:with (bound-id ...) #'(header.name))
  (pattern (define-values ~! (bound-id:id ...) _))
  (pattern (define-syntaxes ~! (bound-id:id ...) _))
  (pattern _ #:with (bound-id ...) #'()))


(define-splicing-syntax-class body-forms
  #:attributes (scopes [bound-id 1] [formatted 1])
  (pattern (~seq form:body-form ...)
    #:with scopes (or (for/first ([b (in-list (reverse (attribute form)))])
                        (scopes-by-location b))
                      (and (pair? (attribute form)) (last (attribute form))))
    #:with (bound-id ...) #'(form.bound-id ... ...)
    #:with (formatted ...) #'((~@ NEWLINE form) ...)))


(module+ test
  (test-case "body-form"
    (define (parse stx) (syntax-parse stx [form:body-form (syntax->datum #'(form.bound-id ...))]))
    (check-equal? (parse #'(define a 42)) (list 'a))
    (check-equal? (parse #'(define (f a b c) 42)) (list 'f))
    (check-equal? (parse #'(define (((f a) b) c) 42)) (list 'f))
    (check-equal? (parse #'(define-syntax a 42)) (list 'a))
    (check-equal? (parse #'(define-syntax (f a b c) 42)) (list 'f))
    (check-equal? (parse #'(define-syntax (((f a) b) c) 42)) (list 'f))
    (check-equal? (parse #'(define-values (a b c) 42)) (list 'a 'b 'c))
    (check-equal? (parse #'(define-syntaxes (a b c) 42)) (list 'a 'b 'c))
    (check-equal? (parse #'(void)) '())))

