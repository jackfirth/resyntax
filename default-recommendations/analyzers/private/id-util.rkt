#lang racket

(require racket/phase+space
         rebellion/type/record
         rebellion/type/singleton
         resyntax/private/syntax-traversal
         syntax/parse
         (only-in racket/base [identifier-binding racket:identifier-binding]))


(define-singleton-type lexical-binding)


(define-record-type local-module-binding
  (name submodule phase))


(define-record-type imported-module-binding
  (export-site-module
   export-site-name
   export-site-phase
   export-site-space
   import-site-phase
   import-site-space))


(define-record-type proxied-module-binding
  (origin-site-module
   origin-site-name
   origin-site-phase
   proxy-site-module
   proxy-site-name
   proxy-site-phase
   proxy-site-space
   import-site-phase
   import-site-space))


(struct top-level-binding (name) #:transparent)


(define-singleton-type unbound)


(define (identifier-binding id-stx
                            #:phase [phase-level (syntax-local-phase-level)]
                            #:exact-scopes? [exact-scopes? #false])
  (match (racket:identifier-binding id-stx phase-level #true exact-scopes?)
    ['lexical lexical-binding]
    [(list top-name) (top-level-binding top-name)]
    [#false unbound]
    [(list from-mod
           from-sym
           nominal-from-mod
           nominal-from-sym
           from-phase
           import-phase+space-shift
           nominal-export-phase+space)
     (cond
       [(self-module-path-index? from-mod)
        (local-module-binding
         #:name from-sym
         #:submodule (module-path-index-submodule from-mod)
         #:phase from-phase)]
       [(equal? from-mod nominal-from-mod)
        (define from-space (phase+space-space nominal-export-phase+space))
        (define-values (phase-shift import-space)
          (match import-phase+space-shift
            [(cons phase-shift new-space) (values phase-shift new-space)]
            [phase-shift (values phase-shift from-space)]))
        (imported-module-binding
         #:export-site-name from-sym
         #:export-site-module from-mod
         #:export-site-phase from-phase
         #:export-site-space from-space
         #:import-site-phase (+ from-phase phase-shift)
         #:import-site-space import-space)]
       [else
        (define nominal-export-phase (phase+space-phase nominal-export-phase+space))
        (define nominal-export-space (phase+space-space nominal-export-phase+space))
        (define-values (phase-shift import-space)
          (match import-phase+space-shift
            [(cons phase-shift new-space) (values phase-shift new-space)]
            [phase-shift (values phase-shift nominal-export-space)]))
        (proxied-module-binding
         #:origin-site-name from-sym
         #:origin-site-module from-mod
         #:origin-site-phase from-phase
         #:proxy-site-name nominal-from-sym
         #:proxy-site-module nominal-from-mod
         #:proxy-site-phase nominal-export-phase
         #:proxy-site-space nominal-export-space
         #:import-site-phase (+ nominal-export-phase phase-shift)
         #:import-site-space import-space)])]))


(define (identifier-binding-map id-stx #:exact-scopes? [exact-scopes? #false])
  (for/hash
      ([phase (in-list (syntax-bound-phases id-stx))]
       #:do [(define binding (identifier-binding id-stx #:phase phase #:exact-scopes? exact-scopes?))]
       #:unless (or (top-level-binding? binding) (unbound? binding)))
    (values phase binding)))


(define (self-module-path-index? mpi)
  (define-values (path base) (module-path-index-split mpi))
  (and (not path) (not base)))


(define expstx
  (expand
   #'(module foo racket
       (module m1 racket
         (define x 1)
         (provide x))
       (module m2 racket
         (require (submod ".." m1))
         (provide x))
       (module m3 racket
         (require (submod ".." m2))
         (void x)))))

(for ([x (in-stream (syntax-search expstx [(~datum x)]))])
  (pretty-print (identifier-binding x)))
