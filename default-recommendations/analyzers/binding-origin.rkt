#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [binding-origin-analyzer expansion-analyzer?]))


(require racket/match
         racket/treelist
         rebellion/streaming/reducer
         resyntax/default-recommendations/analyzers/private/expansion-identifier
         resyntax/private/analyzer
         resyntax/private/syntax-property-bundle)


;@----------------------------------------------------------------------------------------------------


(define (binding-origin stx)
  (for/reducer into-syntax-property-bundle
               ([id (in-treelist (expanded-syntax-identifiers stx))]
                #:when (equal? (expansion-identifier-kind id) 'usage)
                #:do [(define origin (expansion-identifier-binding-origin id))]
                #:when origin)
    (syntax-property-entry (expansion-identifier-path id) 'binding-origin origin)))


(define (expansion-identifier-binding-origin exp-id)
  (match (identifier-binding (expansion-identifier-syntax exp-id) (expansion-identifier-phase exp-id))
    ['lexical 'lexical]
    [_ #false]))


(define binding-origin-analyzer
  (make-expansion-analyzer binding-origin #:name 'binding-origin-analyzer))
