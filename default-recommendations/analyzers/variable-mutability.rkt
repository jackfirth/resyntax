#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [variable-mutability-analyzer expansion-analyzer?]))


(require racket/list
         racket/match
         racket/stream
         rebellion/collection/entry
         rebellion/streaming/transducer
         resyntax/default-recommendations/analyzers/private/expanded-id-table
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

      [(define-syntaxes-id:define-syntaxes ids expr)
       (define new-define-syntaxes (loop (attribute define-syntaxes-id) phase #false))
       (define new-ids (loop (attribute ids) phase #true))
       (define new-expr (loop (attribute expr) (add1 phase) #false))
       (define new-datum (list new-define-syntaxes new-ids new-expr))
       (datum->syntax this-syntax new-datum this-syntax this-syntax)]

      [((~or module module*) _ ...) (loop this-syntax 0 #true)]

      #:parent-context-modifier (λ (stx) stx)
      #:parent-srcloc-modifier (λ (stx) stx)
      #:parent-props-modifier (λ (stx) stx))))


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

      [(quote-syntax _ ...) (stream)]

      [(define-values (id ...) body)
       (stream-append (attribute id) (recur (attribute body)))]

      [(define-syntaxes (id ...) body)
       (stream-append (attribute id) (loop (attribute body) (add1 phase)))]

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
      [(quote-syntax _ ...) (stream)]
      [(set! id:id expr)
       (stream-cons (attribute id) (mutated-variables (attribute expr)))])))


(define (variable-mutability stx)
  (define labeled-stx (syntax-label-id-phases (syntax-label-paths stx 'expanded-path)))
  (define variable-table (make-expanded-id-table))
  (for ([id (in-stream (binding-site-variables labeled-stx))])
    (define phase (syntax-property id 'phase))
    (expanded-id-table-set! variable-table (expanded-identifier id phase) 'immutable))
  (for ([id (in-stream (mutated-variables labeled-stx))])
    (define phase (syntax-property id 'phase))
    (expanded-id-table-set! variable-table (expanded-identifier id phase) 'mutable))
  (transduce (in-expanded-id-table variable-table)
             (mapping
              (λ (e)
                (define expanded-id (entry-key e))
                (define mode (entry-value e))
                (define id (expanded-identifier-syntax expanded-id))
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
      (check-equal? props (syntax-property-bundle)))))
