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
         resyntax/private/logger
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
  (define id-stx (expansion-identifier-syntax exp-id))
  (define phase (expansion-identifier-phase exp-id))
  (define mod (expansion-identifier-enclosing-module exp-id))
  (define binding (identifier-binding id-stx phase #false #false))
  (match binding
    ['lexical 'lexical]
    [(list defining-mod _ ...)
     (define-values (path base) (module-path-index-split defining-mod))
     (cond
       [(and (not base) (not path)) 'local-module]
       [else 'required-module])]
    [_
     (log-resyntax-debug "binding for ~a: ~a" id-stx binding)
     #false]))


(define binding-origin-analyzer
  (make-expansion-analyzer binding-origin #:name 'binding-origin-analyzer))
