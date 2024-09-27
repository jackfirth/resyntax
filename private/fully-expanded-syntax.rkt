#lang racket/base


(require racket/contract/base)


(provide
 fully-expanded-top-level-form
 fully-expanded-module-level-form
 fully-expanded-expression
 (contract-out
  [fully-expanded-syntax-binding-table
   (-> syntax? (free-id-table/c identifier? (listof identifier?) #:immutable #true))]))


(require racket/list
         racket/treelist
         syntax/id-table
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(define-syntax-class (fully-expanded-top-level-form [phase 0])
  #:attributes (bound-ids used-ids)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (~var subform (fully-expanded-general-top-level-form phase))
    #:attr bound-ids (attribute subform.bound-ids)
    #:attr used-ids (attribute subform.used-ids))

  (pattern (#%expression (~var subexpr (fully-expanded-expression phase)))
    #:attr bound-ids (attribute subexpr.bound-ids)
    #:attr used-ids (attribute subexpr.used-ids))

  (pattern (module :id :module-path
             (#%plain-module-begin (~var body (fully-expanded-module-level-form phase)) ...))
    #:attr bound-ids (apply treelist-append (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids)))

  (pattern (begin (~var body (fully-expanded-top-level-form phase)) ...)
    #:attr bound-ids (apply treelist-append (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids)))

  (pattern (begin-for-syntax (~var body (fully-expanded-top-level-form (add1 phase))) ...)
    #:attr bound-ids (apply treelist-append (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids))))


(define-syntax-class (fully-expanded-module-level-form phase)
  #:attributes (bound-ids used-ids)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (~var subform (fully-expanded-general-top-level-form phase))
    #:attr bound-ids (attribute subform.bound-ids)
    #:attr used-ids (attribute subform.used-ids))

  (pattern (#%provide :raw-provide-spec ...)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist))

  (pattern (begin-for-syntax (~var body (fully-expanded-module-level-form (add1 phase))) ...)
    #:attr bound-ids (apply treelist-append (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids)))

  (pattern (~var subform (fully-expanded-submodule-form phase))
    #:attr bound-ids (attribute subform.bound-ids)
    #:attr used-ids (attribute subform.used-ids))

  (pattern (#%declare _ ...)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist)))


(define-syntax-class (fully-expanded-submodule-form phase)
  #:attributes (bound-ids used-ids)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (module :id :module-path
             (#%plain-module-begin (~var body (fully-expanded-module-level-form phase)) ...))
    #:attr bound-ids (apply treelist-append (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids)))

  (pattern (module* :id (~or #false :module-path)
             (#%plain-module-begin (~var body (fully-expanded-module-level-form phase)) ...))
    #:attr bound-ids (apply treelist-append (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids))))


(define-syntax-class (fully-expanded-general-top-level-form phase)
  #:attributes (bound-ids used-ids)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern (~var subexpr (fully-expanded-expression phase))
    #:attr bound-ids (attribute subexpr.bound-ids)
    #:attr used-ids (attribute subexpr.used-ids))

  (pattern (define-values (id:id ...) (~var rhs (fully-expanded-expression phase)))
    #:attr bound-ids (treelist-append (list->treelist (attribute id)) (attribute rhs.bound-ids))
    #:attr used-ids (attribute rhs.used-ids))

  (pattern (define-syntaxes (id:id ...) (~var rhs (fully-expanded-expression (add1 phase))))
    #:attr bound-ids (treelist-append (list->treelist (attribute id)) (attribute rhs.bound-ids))
    #:attr used-ids (attribute rhs.used-ids))

  (pattern (#%require :raw-require-spec ...)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist)))


(define-syntax-class (fully-expanded-expression phase)
  #:attributes (bound-ids used-ids)
  #:literal-sets ((kernel-literals #:phase phase))

  (pattern id:id
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist (attribute id)))

  (pattern (#%plain-lambda formals:fully-expanded-formals
                           (~var body (fully-expanded-expression phase)) ...+)
    #:attr bound-ids (apply treelist-append (attribute formals.bound-ids) (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids)))

  (pattern (case-lambda
             (formals:fully-expanded-formals (~var body (fully-expanded-expression phase)) ...+) ...)
    #:attr bound-ids
    (apply treelist-append (append* (attribute formals.bound-ids) (attribute body.bound-ids)))
    #:attr used-ids (apply treelist-append (append* (attribute body.used-ids))))

  (pattern (if (~var condition (fully-expanded-expression phase))
               (~var true-branch (fully-expanded-expression phase))
               (~var false-branch (fully-expanded-expression phase)))
    #:attr bound-ids
    (treelist-append (attribute condition.bound-ids)
                     (attribute true-branch.bound-ids)
                     (attribute false-branch.bound-ids))
    #:attr used-ids
    (treelist-append (attribute condition.used-ids)
                     (attribute true-branch.used-ids)
                     (attribute false-branch.used-ids)))

  (pattern (begin (~var body (fully-expanded-expression phase)) ...+)
    #:attr bound-ids (apply treelist-append (attribute body.bound-ids))
    #:attr used-ids (apply treelist-append (attribute body.used-ids)))

  (pattern (begin0 (~var result (fully-expanded-expression phase))
             (~var post-body (fully-expanded-expression phase)) ...)
    #:attr bound-ids
    (apply treelist-append (attribute result.bound-ids) (attribute post-body.bound-ids))
    #:attr used-ids
    (apply treelist-append (attribute result.used-ids) (attribute post-body.used-ids)))

  (pattern (let-values ([(id:id ...) (~var rhs (fully-expanded-expression phase))] ...)
             (~var body (fully-expanded-expression phase)) ...+)
    #:do [(define immediately-bound-ids (list->treelist (append* (attribute id))))]
    #:attr bound-ids
    (apply treelist-append
           immediately-bound-ids
           (append (attribute rhs.bound-ids) (attribute body.bound-ids)))
    #:attr used-ids
    (apply treelist-append (append (attribute rhs.used-ids) (attribute body.used-ids))))

  (pattern (letrec-values ([(id:id ...) (~var rhs (fully-expanded-expression phase))] ...)
             (~var body (fully-expanded-expression phase)) ...+)
    #:do [(define immediately-bound-ids (list->treelist (append* (attribute id))))]
    #:attr bound-ids
    (apply treelist-append
           immediately-bound-ids
           (append (attribute rhs.bound-ids) (attribute body.bound-ids)))
    #:attr used-ids
    (apply treelist-append (append (attribute rhs.used-ids) (attribute body.used-ids))))

  (pattern (set! id:id (~var rhs (fully-expanded-expression phase)))
    #:attr bound-ids (attribute rhs.bound-ids)
    #:attr used-ids
    (treelist-cons (attribute rhs.used-ids) (attribute id)))

  (pattern (quote _)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist))

  (pattern (quote-syntax _)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist))

  (pattern (quote-syntax _ #:local)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist))

  (pattern (with-continuation-mark
               (~var key (fully-expanded-expression phase))
             (~var value (fully-expanded-expression phase))
             (~var result (fully-expanded-expression phase)))
    #:attr bound-ids
    (treelist-append (attribute key.bound-ids)
                     (attribute value.bound-ids)
                     (attribute result.bound-ids))
    #:attr used-ids
    (treelist-append (attribute key.used-ids)
                     (attribute value.used-ids)
                     (attribute result.used-ids)))

  (pattern (#%plain-app (~var subexpr (fully-expanded-expression phase)) ...+)
    #:attr bound-ids (apply treelist-append (attribute subexpr.bound-ids))
    #:attr used-ids (apply treelist-append (attribute subexpr.used-ids)))

  (pattern (#%top . id:id)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist (attribute id)))

  (pattern (#%variable-reference id:id)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist (attribute id)))

  (pattern (#%variable-reference (#%top . id:id))
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist (attribute id)))

  (pattern (#%variable-reference)
    #:attr bound-ids (treelist)
    #:attr used-ids (treelist)))


(define-syntax-class fully-expanded-formals
  #:attributes (bound-ids used-ids)

  (pattern (id:id ...)
    #:attr bound-ids (list->treelist (attribute id))
    #:attr used-ids (treelist))

  (pattern (id:id ...+ . rest-id:id)
    #:attr bound-ids (list->treelist (attribute id))
    #:attr used-ids (treelist))

  (pattern id:id
    #:attr bound-ids (treelist (attribute id))
    #:attr used-ids (treelist)))


(define-syntax-class module-path
  (pattern _))


(define-syntax-class raw-require-spec
  (pattern _))


(define-syntax-class raw-provide-spec
  (pattern _))


(define (identifier-binding-table bound-ids used-ids)
  (for*/fold ([map (make-immutable-free-id-table)])
             ([bound bound-ids]
              [used used-ids]
              #:when (free-identifier=? bound used))
    (free-id-table-update map bound (λ (previous) (cons used previous)) '())))


(define (fully-expanded-syntax-binding-table stx)
  (syntax-parse stx
    [:fully-expanded-top-level-form
     (identifier-binding-table (attribute bound-ids) (attribute used-ids))]))
