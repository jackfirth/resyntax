#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [fully-expanded-syntax-binding-table
   (-> syntax?
       (hash/c (or/c exact-integer? #false)
               (free-id-table/c identifier? (listof identifier?) #:immutable #true)
               #:immutable #true))]
  [fully-expanded-syntax-disappeared-visits
   (-> syntax? (vectorof syntax? #:immutable #true #:flat? #true))]))


(require racket/hash
         racket/list
         racket/match
         racket/set
         racket/treelist
         rebellion/collection/hash
         rebellion/collection/vector/builder
         resyntax/private/syntax-traversal
         syntax/id-table
         syntax/parse)


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
    #:attr bound-ids-by-phase (hash phase (list->treelist (attribute id)))
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


(define (fully-expanded-syntax-disappeared-visits stx)
  (define builder (make-vector-builder))
  (let loop ([stx stx])
    (syntax-traverse stx
      [form
       #:when (syntax-property #'form 'disappeared-visit)
       (vector-builder-add-cons-tree builder (syntax-property #'form 'disappeared-visit))
       (loop (syntax-property #'form 'disappeared-visit #false))]))
  (build-vector builder))


(define (vector-builder-add-cons-tree builder cons-tree)
  (match cons-tree
    [(cons left right)
     (vector-builder-add-cons-tree builder left)
     (vector-builder-add-cons-tree builder right)]
    ['() builder]
    [_
     (vector-builder-add builder cons-tree)]))
