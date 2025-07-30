#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [identifier-usage-analyzer expansion-analyzer?]))


(require racket/hash
         racket/list
         racket/set
         racket/stream
         racket/treelist
         rebellion/collection/hash
         rebellion/streaming/transducer
         resyntax/private/analyzer
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         syntax/id-table
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/collection/sorted-map
           rebellion/collection/sorted-set))


;@----------------------------------------------------------------------------------------------------


(define (append-all-id-maps id-maps)
  (for/fold ([combined (hash)])
            ([map id-maps])
    (hash-union combined map #:combine treelist-append)))


(define (id-map-shift-phase id-map levels)
  (for/hash ([(phase ids) (in-hash id-map)])
    (values (and phase (+ phase levels)) ids)))


(define-syntax-class (fully-expanded-top-level-form [phase 0])
  #:attributes (bound-ids-by-phase used-ids-by-phase)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (~var subform (fully-expanded-general-top-level-form phase))
    #:attr bound-ids-by-phase (attribute subform.bound-ids-by-phase)
    #:attr used-ids-by-phase (attribute subform.used-ids-by-phase))

  (pattern (#%expression (~var subexpr (fully-expanded-expression phase)))
    #:attr bound-ids-by-phase (attribute subexpr.bound-ids-by-phase)
    #:attr used-ids-by-phase (attribute subexpr.used-ids-by-phase))

  (pattern (module :id :module-path
             (#%plain-module-begin (~var body (fully-expanded-module-level-form phase)) ...))
    #:attr bound-ids-by-phase (append-all-id-maps (attribute body.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase)))

  (pattern (begin (~var body (fully-expanded-top-level-form phase)) ...)
    #:attr bound-ids-by-phase (append-all-id-maps (attribute body.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase)))

  (pattern (begin-for-syntax (~var body (fully-expanded-top-level-form (add1 phase))) ...)
    #:attr bound-ids-by-phase (append-all-id-maps (attribute body.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase))))


(define-syntax-class (fully-expanded-module-level-form phase)
  #:attributes (bound-ids-by-phase used-ids-by-phase)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (~var subform (fully-expanded-general-top-level-form phase))
    #:attr bound-ids-by-phase (attribute subform.bound-ids-by-phase)
    #:attr used-ids-by-phase (attribute subform.used-ids-by-phase))

  (pattern (#%provide :raw-provide-spec ...)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash))

  (pattern (begin-for-syntax (~var body (fully-expanded-module-level-form (add1 phase))) ...)
    #:attr bound-ids-by-phase (append-all-id-maps (attribute body.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase)))

  (pattern (~var subform (fully-expanded-submodule-form phase))
    #:attr bound-ids-by-phase (attribute subform.bound-ids-by-phase)
    #:attr used-ids-by-phase (attribute subform.used-ids-by-phase))

  (pattern (#%declare _ ...)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash)))


(define-syntax-class (fully-expanded-submodule-form phase)
  #:attributes (bound-ids-by-phase used-ids-by-phase)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (module :id :module-path
             (#%plain-module-begin (~var body (fully-expanded-module-level-form phase)) ...))
    #:attr bound-ids-by-phase (append-all-id-maps (attribute body.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase)))

  (pattern (module* :id (~or #false :module-path)
             (#%plain-module-begin (~var body (fully-expanded-module-level-form phase)) ...))
    #:attr bound-ids-by-phase (append-all-id-maps (attribute body.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase))))


(define-syntax-class (fully-expanded-general-top-level-form phase)
  #:attributes (bound-ids-by-phase used-ids-by-phase)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (~var subexpr (fully-expanded-expression phase))
    #:attr bound-ids-by-phase (attribute subexpr.bound-ids-by-phase)
    #:attr used-ids-by-phase (attribute subexpr.used-ids-by-phase))

  (pattern (define-values (id:id ...) (~var rhs (fully-expanded-expression phase)))
    #:attr bound-ids-by-phase
    (append-all-id-maps (list (hash phase (list->treelist (attribute id)))
                              (attribute rhs.bound-ids-by-phase)))
    #:attr used-ids-by-phase (attribute rhs.used-ids-by-phase))

  (pattern (define-syntaxes (id:id ...) (~var rhs (fully-expanded-expression (add1 phase))))
    #:attr bound-ids-by-phase
    (append-all-id-maps (list (hash phase (list->treelist (attribute id)))
                              (attribute rhs.bound-ids-by-phase)))
    #:attr used-ids-by-phase (attribute rhs.used-ids-by-phase))

  (pattern (#%require :raw-require-spec ...)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash)))


(define-syntax-class (fully-expanded-expression phase)
  #:attributes (bound-ids-by-phase used-ids-by-phase)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern id:id
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash phase (treelist (attribute id))))

  (pattern (#%plain-lambda (~var formals (fully-expanded-formals phase))
                           (~var body (fully-expanded-expression phase)) ...+)
    #:attr bound-ids-by-phase
    (append-all-id-maps (cons (attribute formals.bound-ids-by-phase)
                              (attribute body.bound-ids-by-phase)))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase)))

  (pattern (case-lambda
             ((~var formals (fully-expanded-formals phase))
              (~var body (fully-expanded-expression phase)) ...+)
             ...)
    #:attr bound-ids-by-phase
    (append-all-id-maps (append* (attribute formals.bound-ids-by-phase)
                                 (attribute body.bound-ids-by-phase)))
    #:attr used-ids-by-phase (append-all-id-maps (append* (attribute body.used-ids-by-phase))))

  (pattern (if (~var condition (fully-expanded-expression phase))
               (~var true-branch (fully-expanded-expression phase))
               (~var false-branch (fully-expanded-expression phase)))
    #:attr bound-ids-by-phase
    (append-all-id-maps
     (list (attribute condition.bound-ids-by-phase)
           (attribute true-branch.bound-ids-by-phase)
           (attribute false-branch.bound-ids-by-phase)))
    #:attr used-ids-by-phase
    (append-all-id-maps
     (list (attribute condition.used-ids-by-phase)
           (attribute true-branch.used-ids-by-phase)
           (attribute false-branch.used-ids-by-phase))))

  (pattern (begin (~var body (fully-expanded-expression phase)) ...+)
    #:attr bound-ids-by-phase (append-all-id-maps (attribute body.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute body.used-ids-by-phase)))

  (pattern (begin0 (~var result (fully-expanded-expression phase))
             (~var post-body (fully-expanded-expression phase)) ...)
    #:attr bound-ids-by-phase
    (append-all-id-maps
     (cons (attribute result.bound-ids-by-phase) (attribute post-body.bound-ids-by-phase)))
    #:attr used-ids-by-phase
    (append-all-id-maps
     (cons (attribute result.used-ids-by-phase) (attribute post-body.used-ids-by-phase))))

  (pattern (let-values ([(id:id ...) (~var rhs (fully-expanded-expression phase))] ...)
             (~var body (fully-expanded-expression phase)) ...+)
    #:do [(define immediately-bound-ids (list->treelist (append* (attribute id))))]
    #:attr bound-ids-by-phase
    (append-all-id-maps
     (append (list (hash phase immediately-bound-ids))
             (attribute rhs.bound-ids-by-phase)
             (attribute body.bound-ids-by-phase)))
    #:attr used-ids-by-phase
    (append-all-id-maps
     (append (attribute rhs.used-ids-by-phase) (attribute body.used-ids-by-phase))))

  (pattern (letrec-values ([(id:id ...) (~var rhs (fully-expanded-expression phase))] ...)
             (~var body (fully-expanded-expression phase)) ...+)
    #:do [(define immediately-bound-ids (list->treelist (append* (attribute id))))]
    #:attr bound-ids-by-phase
    (append-all-id-maps
     (append (list (hash phase immediately-bound-ids))
             (attribute rhs.bound-ids-by-phase)
             (attribute body.bound-ids-by-phase)))
    #:attr used-ids-by-phase
    (append-all-id-maps
     (append (attribute rhs.used-ids-by-phase) (attribute body.used-ids-by-phase))))

  (pattern (set! id:id (~var rhs (fully-expanded-expression phase)))
    #:attr bound-ids-by-phase (attribute rhs.bound-ids-by-phase)
    #:attr used-ids-by-phase
    (append-all-id-maps
     (list (hash phase (treelist (attribute id))) (attribute rhs.used-ids-by-phase))))

  (pattern (quote _)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash))

  (pattern (quote-syntax _)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash))

  (pattern (quote-syntax _ #:local)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash))

  (pattern (with-continuation-mark
               (~var key (fully-expanded-expression phase))
             (~var value (fully-expanded-expression phase))
             (~var result (fully-expanded-expression phase)))
    #:attr bound-ids-by-phase
    (append-all-id-maps
     (list (attribute key.bound-ids-by-phase)
           (attribute value.bound-ids-by-phase)
           (attribute result.bound-ids-by-phase)))
    #:attr used-ids-by-phase
    (append-all-id-maps
     (list (attribute key.used-ids-by-phase)
           (attribute value.used-ids-by-phase)
           (attribute result.used-ids-by-phase))))

  (pattern (#%plain-app (~var subexpr (fully-expanded-expression phase)) ...+)
    #:attr bound-ids-by-phase (append-all-id-maps (attribute subexpr.bound-ids-by-phase))
    #:attr used-ids-by-phase (append-all-id-maps (attribute subexpr.used-ids-by-phase)))

  (pattern (#%top . id:id)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash phase (treelist (attribute id))))

  (pattern (#%variable-reference id:id)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash phase (treelist (attribute id))))

  (pattern (#%variable-reference (#%top . id:id))
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash phase (treelist (attribute id))))

  (pattern (#%variable-reference)
    #:attr bound-ids-by-phase (hash)
    #:attr used-ids-by-phase (hash)))


(define-syntax-class (fully-expanded-formals phase)
  #:attributes (bound-ids-by-phase used-ids-by-phase)

  (pattern (id:id ...)
    #:attr bound-ids-by-phase (hash phase (list->treelist (attribute id)))
    #:attr used-ids-by-phase (hash))

  (pattern (id:id ...+ . rest-id:id)
    #:attr bound-ids-by-phase
    (hash phase (treelist-add (list->treelist (attribute id)) (attribute rest-id)))
    #:attr used-ids-by-phase (hash))

  (pattern id:id
    #:attr bound-ids-by-phase (hash phase (treelist (attribute id)))
    #:attr used-ids-by-phase (hash)))


(define-syntax-class module-path
  (pattern _))


(define-syntax-class raw-require-spec
  (pattern _))


(define-syntax-class raw-provide-spec
  (pattern _))


(define (phase-binding-table bound-ids used-ids #:phase phase)
  (for*/fold ([map (make-immutable-free-id-table #:phase phase)])
             ([bound bound-ids]
              [used used-ids]
              #:when (free-identifier=? bound used))
    (free-id-table-update map bound (λ (previous) (cons used previous)) '())))


(define (identifier-binding-table bound-ids-by-phase used-ids-by-phase)
  (for/hash
      ([phase
        (in-set (set-union (hash-key-set bound-ids-by-phase) (hash-key-set used-ids-by-phase)))])
    (define bound-ids (hash-ref bound-ids-by-phase phase '()))
    (define used-ids (hash-ref used-ids-by-phase phase '()))
    (values phase (phase-binding-table bound-ids used-ids #:phase phase))))


(define (fully-expanded-syntax-binding-table stx)
  (syntax-parse stx
    [:fully-expanded-top-level-form
     (identifier-binding-table (attribute bound-ids-by-phase) (attribute used-ids-by-phase))]))


(define identifier-usage-analyzer
  (make-expansion-analyzer
   #:name 'identifier-usage-analyzer
   (λ (expanded-stx)
     (define table
       (fully-expanded-syntax-binding-table (syntax-label-paths expanded-stx 'expanded-path)))
     (transduce (in-hash-values table)
                (append-mapping
                 (λ (id-table)
                   (for/stream ([(bound-id usages) (in-free-id-table id-table)])
                     (define exp-path (syntax-property bound-id 'expanded-path))
                     (syntax-property-entry exp-path 'identifier-usages usages))))
                #:into into-syntax-property-bundle))))


(module+ test
  (test-case "identifier-usage-analyzer"
    ;; Arrange
    (define stx
      #'(module foo racket/base
          (let ([x 42] [y 42])
            (+ x y))))
    (define expanded
      (parameterize ([current-namespace (make-base-namespace)])
        (expand stx)))
    (define expanded-x-path (syntax-path (treelist 3 2 2 2 1 0 0 0)))
    (define expanded-y-path (syntax-path (treelist 3 2 2 2 1 1 0 0)))
    (check-equal? (syntax->datum (syntax-ref expanded expanded-x-path)) 'x)
    (check-equal? (syntax->datum (syntax-ref expanded expanded-y-path)) 'y)
    (define x-binding-clause-path (syntax-path-parent (syntax-path-parent expanded-x-path)))
    (define y-binding-clause-path (syntax-path-parent (syntax-path-parent expanded-y-path)))
    (check-equal? (syntax->datum (syntax-ref expanded x-binding-clause-path)) '[(x) '42])
    (check-equal? (syntax->datum (syntax-ref expanded y-binding-clause-path)) '[(y) '42])

    ;; Act
    (define props (expansion-analyze identifier-usage-analyzer expanded))
    (define prop-map (syntax-property-bundle-as-map props))

    ;; Assert
    (define expected-keys (sorted-set expanded-x-path expanded-y-path #:comparator syntax-path<=>))
    (check-equal? (sorted-map-keys prop-map) expected-keys)
    (check-equal? (length (hash-ref (sorted-map-get prop-map expanded-x-path) 'identifier-usages)) 1)
    (check-equal? (length (hash-ref (sorted-map-get prop-map expanded-y-path) 'identifier-usages))
                  1)))
