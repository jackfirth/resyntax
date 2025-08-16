#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [variable-mutability-analyzer expansion-analyzer?]))


(require racket/dict
         racket/list
         racket/match
         racket/stream
         racket/treelist
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         resyntax/default-recommendations/analyzers/private/expansion-identifier
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


(define (variable-mutability stx)
  (define ids (expanded-syntax-identifiers stx))
  (define variable-table (make-hash))
  (for ([id (in-treelist ids)]
        #:when (equal? (expansion-identifier-kind id) 'binding))
    (define phase (expansion-identifier-phase id))
    (define phase-specific-table
      (hash-ref! variable-table phase (λ () (make-free-id-table #:phase phase))))
    (free-id-table-set! phase-specific-table (expansion-identifier-syntax id) 'immutable))
  (for ([id (in-treelist ids)]
        #:do [(match-define (expansion-identifier id-stx path phase mod-names kind) id)]
        #:when (and (equal? kind 'usage)
                    (not (equal? path empty-syntax-path))
                    (equal? (treelist-last (syntax-path-elements path)) 1)))
    (syntax-parse (syntax-ref stx (syntax-path-parent path))
      #:literals (set!)
      [(set! _ _)
       (define phase-specific-table
         (hash-ref! variable-table phase (λ () (make-free-id-table #:phase phase))))
       (free-id-table-set! phase-specific-table id-stx 'mutable)]
      [_ (void)]))
  (for/reducer into-syntax-property-bundle
               ([id (in-treelist ids)]
                #:when (equal? (expansion-identifier-kind id) 'binding))
    (match-define (expansion-identifier id-stx path phase _ _) id)
    (define mode (free-id-table-ref (hash-ref variable-table phase) id-stx))
    (syntax-property-entry path 'variable-mutability mode)))


(define variable-mutability-analyzer
  (make-expansion-analyzer variable-mutability #:name 'variable-mutability-analyzer))


(module+ test
  (test-case "variable-mutability-analyzer"

    (test-case "empty module"
      (define stx #'(module foo racket/base))
      (define props (expansion-analyze variable-mutability-analyzer (expand stx)))
      (check-equal? props (syntax-property-bundle)))))
