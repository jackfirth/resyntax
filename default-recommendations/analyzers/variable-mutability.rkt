#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [variable-mutability-analyzer expansion-analyzer?]))


(require racket/dict
         racket/list
         racket/match
         racket/stream
         rebellion/streaming/transducer
         resyntax/private/analyzer
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-traversal
         syntax/id-table
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (syntax-label-id-phases expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0] [skip? #false])
    (syntax-traverse expanded-stx
      #:skip-root? skip?
      #:literal-sets ([kernel-literals #:phase phase])
      [:id (syntax-property this-syntax 'phase phase)]
      [(begin-for-syntax _ ...) (loop this-syntax (add1 phase) #true)]
      [((~or module module*) _ ...) (loop this-syntax 0 #true)])))


(define (binding-site-variables expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0])
    (define (recur stx)
      (loop stx phase))
    (syntax-search expanded-stx
      #:literal-sets ([kernel-literals #:phase phase])

      [(id:id _ ...)
       #:do [(define id-phase (syntax-property (attribute id) 'phase))]
       #:when (not (equal? id-phase phase))
       (loop this-syntax id-phase)]

      [(define-values (id ...) body)
       (stream-append (attribute id) (recur (attribute body)))]

      [((~or let-values letrec-values) ([(id ...) rhs] ...) body ...)
       (define inner-exprs (append (attribute rhs) (attribute body)))
       (define ids (append* (attribute id)))
       (apply stream-append ids (map recur inner-exprs))]

      [(#%plain-lambda formals body ...)
       (apply stream-append
              (syntax-search (attribute formals) [:id])
              (map recur (attribute body)))]

      [(case-lambda [formals body ...] ...)
       (apply stream-append
              (syntax-search #'(formals ...) [:id])
              (map recur (append* (attribute body))))])))


(define (mutated-variables expanded-stx)
  (let loop ([expanded-stx expanded-stx] [phase 0])
    (syntax-search expanded-stx
      #:literal-sets ([kernel-literals #:phase phase])
      [(id:id _ ...)
       #:do [(define id-phase (syntax-property (attribute id) 'phase))]
       #:when (not (equal? id-phase phase))
       (loop this-syntax id-phase)]
      [(set! id:id expr)
       (stream-cons (attribute id) (mutated-variables (attribute expr)))])))


(define (variable-mutability stx)
  (define labeled-stx (syntax-label-id-phases (syntax-label-paths stx 'expanded-path)))
  (define variable-table (make-hash))
  (for ([id (in-stream (binding-site-variables labeled-stx))])
    (define phase (syntax-property id 'phase))
    (define phase-specific-table
      (hash-ref! variable-table phase (λ () (make-free-id-table #:phase phase))))
    (free-id-table-set! phase-specific-table id 'immutable))
  (for ([id (in-stream (mutated-variables labeled-stx))])
    (define phase-specific-table (hash-ref variable-table (syntax-property id 'phase)))
    (free-id-table-set! phase-specific-table id 'mutable))
  (transduce (in-hash-values variable-table)
             (append-mapping in-dict-pairs)
             (mapping
              (λ (e)
                (match-define (cons id mode) e)
                (define path (syntax-property id 'expanded-path))
                (syntax-property-entry path 'variable-mutability mode)))
             #:into into-syntax-property-bundle))


(define variable-mutability-analyzer
  (make-expansion-analyzer variable-mutability #:name 'variable-mutability-analyzer))


(module+ test
  (test-case "variable-mutability-analyzer"
    
    (test-case "empty module"
        (define stx #'(module foo racket/base))
        (define props (expansion-analyze variable-mutability-analyzer (expand stx)))
        (check-equal? props (syntax-property-bundle)))

    (test-case "one immutable binding"
        (define stx
          #'(module foo racket/base
              (define a 1)))

        (define props (expansion-analyze variable-mutability-analyzer (expand stx)))

        (define expected-props
          (syntax-property-bundle
           (syntax-property-entry (syntax-path (list 3 2 1 0)) 'variable-mutability 'immutable)))
        (check-equal? props expected-props))

    (test-case "one mutable binding"
        (define stx
          #'(module foo racket/base
              (define a 1)
              (set! a 2)))

        (define props (expansion-analyze variable-mutability-analyzer (expand stx)))

        (define expected-props
          (syntax-property-bundle
           (syntax-property-entry (syntax-path (list 3 2 1 0)) 'variable-mutability 'mutable)))
        (check-equal? props expected-props))

    (test-case "one immutable phase 1 binding"
      (define stx
        #'(module foo racket
            (begin-for-syntax
              (define a 1))))

      (define props (expansion-analyze variable-mutability-analyzer (expand stx)))

      (define expected-props
        (syntax-property-bundle
         (syntax-property-entry (syntax-path (list 3 2 1 1 0)) 'variable-mutability 'immutable)))
      (check-equal? props expected-props))

    (test-case "one mutable phase 1 binding"
      (define stx
        #'(module foo racket
            (begin-for-syntax
              (define a 1)
              (set! a 2))))

      (define props (expansion-analyze variable-mutability-analyzer (expand stx)))

      (define expected-props
        (syntax-property-bundle
         (syntax-property-entry (syntax-path (list 3 2 1 1 0)) 'variable-mutability 'mutable)))
      (check-equal? props expected-props))

    (test-case "one immutable phase 2 binding"
      (define stx
        #'(module foo racket
            (require (for-meta 2 racket/base))
            (begin-for-syntax
              (begin-for-syntax
                (define a 1)))))

      (define props (expansion-analyze variable-mutability-analyzer (expand stx)))

      (define expected-props
        (syntax-property-bundle
         (syntax-property-entry (syntax-path (list 3 3 1 1 1 0)) 'variable-mutability 'immutable)))
      (check-equal? props expected-props))

    (test-case "one mutable phase 2 binding"
      (define stx
        #'(module foo racket
            (require (for-meta 2 racket/base))
            (begin-for-syntax
              (begin-for-syntax
                (define a 1)
                (set! a 2)))))

      (define props (expansion-analyze variable-mutability-analyzer (expand stx)))

      (define expected-props
        (syntax-property-bundle
         (syntax-property-entry (syntax-path (list 3 3 1 1 1 0)) 'variable-mutability 'mutable)))
      (check-equal? props expected-props))))
