#lang racket/base


(provide refactorable-let-expression)


(require racket/list
         racket/match
         racket/sequence
         racket/set
         racket/syntax
         racket/vector
         rebellion/base/option
         rebellion/collection/entry
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/graph
         resyntax/syntax-rendering
         syntax/id-set
         syntax/parse
         syntax/parse/lib/function-header
         syntax/stx)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-splicing-syntax-class refactorable-let-expression
  #:attributes ([refactored 1])
  #:literals (let let*)

  (pattern
      (~seq leading-body:body-forms (let ~! bindings:refactorable-let-bindings inner-body:body-forms))
    
    #:when (no-binding-overlap? (syntax-identifiers #'leading-body)
                                (in-syntax #'(bindings.outer-bound-id ...)))
    #:when (no-binding-overlap? (in-syntax #'(inner-body.bound-id ...))
                                (in-syntax #'(bindings.inner-bound-id ...)))
    #:with (refactored ...)
    (if (attribute bindings.fully-refactorable?)
        #'(leading-body.formatted ...
           bindings.outer-definition ...
           inner-body.formatted ...)
        #'(leading-body.formatted ...
           bindings.outer-definition ...
           NEWLINE (let bindings.unrefactorable
                     bindings.inner-definition ...
                     inner-body.formatted ...))))

  (pattern
      (~seq
       leading-body:body-forms (let* ~! bindings:refactorable-let*-bindings inner-body:body-forms))
      
    #:when (no-binding-overlap? (syntax-identifiers #'leading-body)
                                (in-syntax #'(bindings.outer-bound-id ...)))
    #:when (no-binding-overlap? (in-syntax #'(inner-body.bound-id ...))
                                (in-syntax #'(bindings.inner-bound-id ...)))
    #:with (refactored ...)
    (if (attribute bindings.fully-refactorable?)
        #'(leading-body.formatted ...
           bindings.outer-definition ...
           inner-body.formatted ...)
        #'(leading-body.formatted ...
           bindings.outer-definition ...
           NEWLINE (let* bindings.unrefactorable
                     bindings.inner-definition ...
                     inner-body.formatted ...)))))


(module+ test
  (test-case (name-string refactorable-let-expression)

    (define (parse stx)
      (syntax-parse stx
        [(let-expr:refactorable-let-expression) (syntax->datum #'(let-expr.refactored ...))]
        [_ #false]))

    (check-equal? (parse #'((let ([a 1]) a))) '(NEWLINE (define a 1) NEWLINE a))
    (check-equal? (parse #'((let* ([a 1]) a))) '(NEWLINE (define a 1) NEWLINE a))
    (check-false (parse #'((let ([a a]) a))))
    (check-false (parse #'((let* ([a a]) a))))

    (test-case "multiple unrelated let bindings"
      (define stx #'((let ([a 1] [b 2] [c 3]) (+ a b c))))
      (define expected
        '(NEWLINE (define a 1) NEWLINE (define b 2) NEWLINE (define c 3) NEWLINE (+ a b c)))
      (check-equal? (parse stx) expected))

    (test-case "multiple unrelated let* bindings"
      (define stx #'((let* ([a 1] [b 2] [c 3]) (+ a b c))))
      (define expected
        '(NEWLINE (define a 1) NEWLINE (define b 2) NEWLINE (define c 3) NEWLINE (+ a b c)))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings before self-binding"
      (define stx #'((let ([a 1] [b 2] [c c]) (+ a b c))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b 2)
          NEWLINE
          (let ([c c])
            NEWLINE
            (+ a b c))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings before self-binding"
      (define stx #'((let* ([a 1] [b 2] [c c]) (+ a b c))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b 2)
          NEWLINE
          (let* ([c c])
            NEWLINE
            (+ a b c))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings after self-binding"
      (define stx #'((let ([a a] [b 2] [c 3]) (+ a b c))))
      (define expected
        '(NEWLINE
          (let ([a a])
            NEWLINE
            (define b 2)
            NEWLINE
            (define c 3)
            NEWLINE
            (+ a b c))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings after self-binding"
      (define stx #'((let* ([a a] [b 2] [c 3]) (+ a b c))))
      (define expected
        '(NEWLINE
          (let* ([a a])
            NEWLINE
            (define b 2)
            NEWLINE
            (define c 3)
            NEWLINE
            (+ a b c))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings before and after self-binding"
      (define stx #'((let ([a 1] [b b] [c 3]) (+ a b c))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (let ([b b])
            NEWLINE
            (define c 3)
            NEWLINE
            (+ a b c))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings before and after self-binding"
      (define stx #'((let* ([a 1] [b b] [c 3]) (+ a b c))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (let* ([b b])
            NEWLINE
            (define c 3)
            NEWLINE
            (+ a b c))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings before binding cycle"
      (define stx #'((let ([a 1] [b 2] [c d] [d c]) (+ a b c d))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b 2)
          NEWLINE
          (let ([c d] NEWLINE [d c])
            NEWLINE
            (+ a b c d))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings before binding cycle"
      (define stx #'((let* ([a 1] [b 2] [c d] [d c]) (+ a b c d))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b 2)
          NEWLINE
          (define c d)
          NEWLINE
          (let* ([d c])
            NEWLINE
            (+ a b c d))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings after binding cycle"
      (define stx #'((let ([a b] [b a] [c 3] [d 4]) (+ a b c d))))
      (define expected
        '(NEWLINE
          (let ([a b] NEWLINE [b a])
            NEWLINE
            (define c 3)
            NEWLINE
            (define d 4)
            NEWLINE
            (+ a b c d))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings after binding cycle"
      (define stx #'((let* ([a b] [b a] [c 3] [d 4]) (+ a b c d))))
      (define expected
        '(NEWLINE
          (define a b)
          NEWLINE
          (let* ([b a])
            NEWLINE
            (define c 3)
            NEWLINE
            (define d 4)
            NEWLINE
            (+ a b c d))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings before and after binding cycle"
      (define stx #'((let ([a 1] [b c] [c b] [d 4]) (+ a b c d))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (let ([b c] NEWLINE [c b])
            NEWLINE
            (define d 4)
            NEWLINE
            (+ a b c d))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings before and after binding cycle"
      (define stx #'((let* ([a 1] [b c] [c b] [d 4]) (+ a b c d))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b c)
          NEWLINE
          (let* ([c b])
            NEWLINE
            (define d 4)
            NEWLINE
            (+ a b c d))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings before and after binding cycle interspersed with bindings"
      (define stx #'((let ([a 1] [b d] [c 3] [d b] [e 5]) (+ a b c d e))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (let ([b d] NEWLINE [c 3] NEWLINE [d b])
            NEWLINE
            (define e 5)
            NEWLINE
            (+ a b c d e))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings before and after binding cycle interspersed with bindings"
      (define stx #'((let* ([a 1] [b d] [c 3] [d b] [e 5]) (+ a b c d e))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b d)
          NEWLINE
          (define c 3)
          NEWLINE
          (let* ([d b])
            NEWLINE
            (define e 5)
            NEWLINE
            (+ a b c d e))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings before and after multiple binding cycles"
      (define stx #'((let ([a 1] [b c] [c b] [d e] [e d] [f 6]) (+ a b c d e f))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (let ([b c] NEWLINE [c b] NEWLINE [d e] NEWLINE [e d])
            NEWLINE
            (define f 6)
            NEWLINE
            (+ a b c d e f))))
      (check-equal? (parse stx) expected))

    (test-case "refactorable let* bindings before and after multiple binding cycles"
      (define stx #'((let* ([a 1] [b c] [c b] [d e] [e d] [f 6]) (+ a b c d e f))))
      (define expected
        '(NEWLINE
          (define a 1)
          NEWLINE
          (define b c)
          NEWLINE
          (let* ([c b] NEWLINE [d e] NEWLINE [e d])
            NEWLINE
            (define f 6)
            NEWLINE
            (+ a b c d e f))))
      (check-equal? (parse stx) expected))

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

    ;; TODO: get this to work
    #;(test-case "refactorable let bindings after partially conflicting definitions"
        (define stx #'((define a 1) (let ([x 2] [a 3] [y 4]) a)))
        (define expected
          '(NEWLINE
            (define a 1)
            NEWLINE
            (define x 2)
            NEWLINE
            (let ([a 3])
              NEWLINE
              (define y 4)
              NEWLINE
              a)))
        (check-equal? (parse stx) expected))

    ;; TODO: get this to work
    #;(test-case "refactorable let* bindings after partially conflicting definitions"
        (define stx #'((define a 1) (let* ([x 2] [a 3] [y 4]) a)))
        (define expected
          '(NEWLINE
            (define a 1)
            NEWLINE
            (define x 2)
            NEWLINE
            (let* ([a 3])
              NEWLINE
              (define y 4)
              NEWLINE
              a)))
        (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings after capturing definitions"
      (define stx #'((define a x) (let ([x 1]) (+ a x))))
      (check-false (parse stx)))

    (test-case "refactorable let* bindings after capturing definitions"
      (define stx #'((define a x) (let* ([x 1]) (+ a x))))
      (check-false (parse stx)))

    ;; TODO: get this to work
    #;(test-case "refactorable let bindings after partially capturing definitions"
        (define stx #'((define a x) (let ([b 2] [x 3] [c 4]) (+ a b x c))))
        (define expected
          '(NEWLINE
            (define a x)
            NEWLINE
            (define b 2)
            NEWLINE
            (let ([x 3])
              NEWLINE
              (define c 4)
              NEWLINE
              (+ a b x c))))
        (check-equal? (parse stx) expected))

    ;; TODO: get this to work
    #;(test-case "refactorable let* bindings after partially capturing definitions"
        (define stx #'((define a x) (let* ([b 2] [x 3] [c 4]) (+ a b x c))))
        (define expected
          '(NEWLINE
            (define a x)
            NEWLINE
            (define b 2)
            NEWLINE
            (let* ([x 3])
              NEWLINE
              (define c 4)
              NEWLINE
              (+ a b x c))))
        (check-equal? (parse stx) expected))

    (test-case "refactorable let bindings colliding with later self binding"
      (define stx #'((let ([a c] [b c] [c c]) (+ a b c))))
      (define expected
        '(NEWLINE
          (define a c)
          NEWLINE
          (define b c)
          NEWLINE
          (let ([c c])
            NEWLINE
            (+ a b c))))
      (check-equal? (parse stx) expected))))


(define-syntax-class refactorable-let-bindings
  #:attributes ([outer-bound-id 1]
                [inner-bound-id 1]
                [outer-definition 1]
                [inner-definition 1]
                fully-refactorable?
                unrefactorable)
  (pattern (clause:binding-clause ...)
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
  #:attributes ([outer-bound-id 1]
                [inner-bound-id 1]
                [outer-definition 1]
                [inner-definition 1]
                fully-refactorable?
                unrefactorable)
  (pattern (clause:binding-clause ...)
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


(define-record-type parsed-binding-clause
  (original bound-identifier right-hand-side referenced-identifiers))


(define-syntax-class binding-clause
  #:attributes (parsed)
  (pattern (~and original [id:id rhs:expr])
    #:attr parsed
    (parsed-binding-clause
     #:original #'original
     #:bound-identifier #'id
     #:right-hand-side #'rhs
     #:referenced-identifiers (syntax-identifiers #'rhs))))


(define (parsed-binding-clause-definition clause)
  (define id (parsed-binding-clause-bound-identifier clause))
  (define rhs (parsed-binding-clause-right-hand-side clause))
  (if (> (+ (syntax-span id) (syntax-span rhs)) 90) ;; conservative under-estimate
      #`(define #,id NEWLINE #,rhs)
      #`(define #,id #,rhs)))


(define (binding-clause-depends-on? dependant dependency)
  (define dependant-references (parsed-binding-clause-referenced-identifiers dependant))
  (define dependency-id (parsed-binding-clause-bound-identifier dependency))
  (not (no-binding-overlap? dependant-references (list dependency-id))))


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
  (for/list ([before (in-list (split-bindings-before-cycles split))])
    (parsed-binding-clause-bound-identifier before)))


(define (split-bindings-inner-ids split)
  (for/list ([after (in-list (split-bindings-after-cycles split))])
    (parsed-binding-clause-bound-identifier after)))


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
  #:attributes ([bound-id 1] [formatted 1])
  (pattern (~seq form:body-form ...)
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

