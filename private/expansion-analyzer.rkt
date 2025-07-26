#lang racket/base


(require racket/match
         racket/stream
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/sorted-map
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         resyntax/private/syntax-path
         resyntax/private/syntax-traversal
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct syntax-property-bundle (properties-by-path) #:transparent)
(struct syntax-property-entry (path key value) #:transparent)


(define into-syntax-property-bundle
  (into-transduced
   (mapping
    (λ (prop-entry)
      (match-define (syntax-property-entry path k v) prop-entry)
      (entry path (entry k v))))
   (grouping into-hash)
   #:into (reducer-map (into-sorted-map syntax-path<=>) #:range syntax-property-bundle)))


(define (sequence->syntax-property-bundle prop-entry-seq)
  (transduce prop-entry-seq #:into into-syntax-property-bundle))


(module+ test
  (test-case "into-syntax-property-bundle"
    (define prop-entries
      (list (syntax-property-entry (syntax-path (list 0 1 1)) 'quoted? #true)
            (syntax-property-entry (syntax-path (list 0 1 2)) 'quoted? #true)
            (syntax-property-entry (syntax-path (list 0 3)) 'quoted? #true)))
    (define expected
      (syntax-property-bundle
       (sorted-map #:key-comparator syntax-path<=>
                   (syntax-path (list 0 1 1)) (hash 'quoted? #true)
                   (syntax-path (list 0 1 2)) (hash 'quoted? #true)
                   (syntax-path (list 0 3)) (hash 'quoted? #true))))
    (check-equal? (transduce prop-entries #:into into-syntax-property-bundle) expected)))


(struct syntax-analyzer (analysis-function))


(define (ignored-result-values stx)
  (let loop ([stx (syntax-label-paths stx 'expansion-path)])

    (define (mark-result stx mode)
      (define path (syntax-property stx 'expansion-path))
      (stream-cons (syntax-property-entry path 'expression-result mode) (loop stx)))

    (define (mark-all stxs mode)
      (stream-append-all
       (for/list ([stx (in-list stxs)])
         (mark-result stx mode))))

    (syntax-search stx
      #:literal-sets (kernel-literals)

      [((~or define-values define-syntaxes) _ expr)
       (mark-result (attribute expr) 'used)]

      [(#%plain-lambda _ body ... result)
       (stream-append
        (mark-all (attribute body) 'ignored)
        (mark-result (attribute result) 'used))]

      [(case-lambda [_ body ... result] ...)
       (stream-append-all
        (for/list ([bodies (in-list (attribute body))]
                   [result-stx (in-list (attribute result))])
          (stream-append (mark-all bodies 'ignored)
                         (mark-result result-stx 'used))))]

      [(if condition true-branch false-branch)
       (stream-append
        (mark-result (attribute condition) 'used)
        (mark-result (attribute true-branch) 'used)
        (mark-result (attribute false-branch) 'used))]

      [(begin body ... result)
       (stream-append
        (mark-all (attribute body) 'ignored)
        (mark-result (attribute result) 'used))]

      [(begin0 result body ...)
       (stream-append (mark-result (attribute result) 'used)
                      (mark-all (attribute body) 'ignored))]

      [((~or let-values letrec-values) ([_ expr] ...)
         body ...
         result)
       (stream-append (mark-all (attribute expr) 'used)
                      (mark-all (attribute body) 'ignored)
                      (mark-result (attribute result) 'used))]

      [(set! _ expr) (mark-result (attribute expr) 'used)]

      [(with-continuation-mark key val result)
       (stream-append
        (mark-result (attribute key) 'used)
        (mark-result (attribute val) 'used)
        (mark-result (attribute result) 'used))]

      [(#%plain-app func-or-arg ...) (mark-all (attribute func-or-arg) 'used)])))


(define (stream-append-all streams)
  (apply stream-append streams))


(define test-stx
  #'(define (f x)
       (displayln "hi")
       x))

(expand test-stx)
(sequence->syntax-property-bundle (ignored-result-values (expand test-stx)))
