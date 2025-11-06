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
  
  ;; Create table with all bound identifiers initialized to 'immutable
  (define variable-table (fully-expanded-syntax-id-table labeled-stx))
  
  ;; Initialize all entries with 'immutable
  (for ([entry (in-expanded-id-table variable-table)])
    (expanded-id-table-set! variable-table (entry-key entry) 'immutable))
  
  ;; Mark mutated variables as 'mutable
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
