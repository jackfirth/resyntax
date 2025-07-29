#lang racket/base

(require racket/contract/base)

(provide ; Returns a map of sorted sets of syntax paths. Unfortunately I haven't implemented sorted map and
         ; set contracts.
         (contract-out [syntax-movement-table (-> syntax? immutable-sorted-map?)]))

(require racket/stream
         rebellion/collection/entry
         rebellion/collection/sorted-map
         rebellion/collection/sorted-set
         rebellion/streaming/transducer
         resyntax/private/syntax-neighbors
         resyntax/private/syntax-path
         resyntax/private/syntax-traversal
         syntax/parse)

(module+ test
  (require (submod "..")
           rackunit))

;@----------------------------------------------------------------------------------------------------

; Traverses a syntax object `result-stx`, searching for all syntax objects that have a
; syntax-original-path, then returns a table mapping each original path to the set of paths in
; `result-stx` that corresponded to that original path. If `result-stx` is fully expanded syntax, this
; amounts to returning a table mapping each (path-based) position in the original unexpanded syntax to
; the set of positions in the fully expanded syntax which that original syntax expanded into. Note
; that each input path maps to a set of output paths instead of a single path because macros can
; duplicate forms. The returned table is in the form of an immutable sorted map of syntax paths to
; sorted sets of syntax paths.
(define (syntax-movement-table result-stx)

  (define (search parent-stx [include-self? #true])
    (syntax-search parent-stx
                   [child
                    #:do [(define child-stx (attribute child))]
                    #:when (syntax-original-path child-stx)
                    #:when (or include-self? (not (equal? child-stx parent-stx)))
                    (stream-cons child-stx (search child-stx #false))]))

  (transduce (search (syntax-label-paths result-stx 'final-syntax-path))
             (bisecting syntax-original-path (λ (stx) (syntax-property stx 'final-syntax-path)))
             (grouping (into-sorted-set syntax-path<=>))
             #:into (into-sorted-map syntax-path<=>)))

(module+ test
  (test-case "syntax-movement-table smoke test"
    (define orig-stx
      (syntax-label-original-paths #'(module foo racket/base
                                       (void))))
    (define expanded-stx (expand orig-stx))

    (define table (syntax-movement-table expanded-stx))

    (define expected-table
      (sorted-map #:key-comparator syntax-path<=>
                  ; (module ...)
                  empty-syntax-path
                  (sorted-set empty-syntax-path (syntax-path (list 3)) #:comparator syntax-path<=>)
                  ; module
                  (syntax-path (list 0))
                  (sorted-set (syntax-path (list 0)) #:comparator syntax-path<=>)
                  ; foo
                  (syntax-path (list 1))
                  (sorted-set (syntax-path (list 1)) #:comparator syntax-path<=>)
                  ; racket/base
                  (syntax-path (list 2))
                  (sorted-set (syntax-path (list 2)) #:comparator syntax-path<=>)
                  ; (void)
                  (syntax-path (list 3))
                  (sorted-set (syntax-path (list 3 2)) #:comparator syntax-path<=>)
                  ; void
                  (syntax-path (list 3 0))
                  (sorted-set (syntax-path (list 3 2 (tail-syntax 1) 0))
                              #:comparator syntax-path<=>)))

    (check-equal? table expected-table)))
