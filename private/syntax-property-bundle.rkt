#lang racket/base


(require racket/contract/base)


(provide
 (struct-out syntax-property-entry)
 (contract-out
  [syntax-property-bundle (-> syntax-property-entry? ... syntax-property-bundle?)]
  [syntax-property-bundle? (-> any/c boolean?)]
  [syntax-property-bundle-as-map (-> syntax-property-bundle? immutable-sorted-map?)]
  [syntax-property-bundle-entries (-> syntax-property-bundle? (sequence/c syntax-property-entry?))]
  [sequence->syntax-property-bundle (-> (sequence/c syntax-property-entry?) syntax-property-bundle?)]
  [into-syntax-property-bundle (reducer/c syntax-property-entry? syntax-property-bundle?)]
  [syntax-add-all-properties (-> syntax? syntax-property-bundle? syntax?)]))


(require racket/match
         racket/sequence
         racket/stream
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/sorted-map
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         resyntax/private/syntax-path)


(module+ test
  (require (submod "..")
           rackunit
           syntax/parse))


;@----------------------------------------------------------------------------------------------------


(struct syntax-property-bundle (as-map)
  #:omit-define-syntaxes
  #:constructor-name constructor:syntax-property-bundle
  #:transparent)

(struct syntax-property-entry (path key value)
  #:guard (struct-guard/c syntax-path? any/c any/c)
  #:transparent)


(define into-syntax-property-bundle
  (into-transduced
   (mapping
    (λ (prop-entry)
      (match-define (syntax-property-entry path k v) prop-entry)
      (entry path (entry k v))))
   (grouping into-hash)
   #:into (reducer-map (into-sorted-map syntax-path<=>) #:range constructor:syntax-property-bundle)))


(define (sequence->syntax-property-bundle prop-entry-seq)
  (transduce prop-entry-seq #:into into-syntax-property-bundle))


(define (syntax-property-bundle . prop-entries)
  (sequence->syntax-property-bundle prop-entries))


(module+ test
  (define term-01-quoted-prop (syntax-property-entry (syntax-path (list 0 1)) 'quoted? #true))
  (define term-02-quoted-prop (syntax-property-entry (syntax-path (list 0 2)) 'quoted? #true))
  (define term-03-quoted-prop (syntax-property-entry (syntax-path (list 0 3)) 'quoted? #true))
  (test-case "syntax-property-bundle"
    (define actual
      (syntax-property-bundle term-01-quoted-prop term-02-quoted-prop term-03-quoted-prop))
    (define expected
      (constructor:syntax-property-bundle
       (sorted-map #:key-comparator syntax-path<=>
                   (syntax-path (list 0 1)) (hash 'quoted? #true)
                   (syntax-path (list 0 2)) (hash 'quoted? #true)
                   (syntax-path (list 0 3)) (hash 'quoted? #true))))
    (check-equal? actual expected))

  (test-case "sequence->syntax-property-bundle"
    (define actual
      (sequence->syntax-property-bundle
       (list term-03-quoted-prop term-01-quoted-prop term-02-quoted-prop)))
    (define expected
      (syntax-property-bundle term-01-quoted-prop term-02-quoted-prop term-03-quoted-prop))
    (check-equal? actual expected))

  (test-case "into-syntax-property-bundle"
    (define actual
      (transduce (vector term-01-quoted-prop term-02-quoted-prop term-03-quoted-prop)
                 #:into into-syntax-property-bundle))
    (define expected
      (syntax-property-bundle term-01-quoted-prop term-02-quoted-prop term-03-quoted-prop))
    (check-equal? actual expected)))


(define (syntax-property-bundle-entries prop-bundle)
  (for*/stream ([e (in-sorted-map (syntax-property-bundle-as-map prop-bundle))]
                #:do [(match-define (entry path props) e)]
                [(k v) (in-hash props)])
    (syntax-property-entry path k v)))


(define (syntax-add-all-properties stx prop-bundle)
  (for/fold ([stx stx])
            ([e (in-sorted-map (syntax-property-bundle-as-map prop-bundle))])
    (match-define (entry path props) e)
    (syntax-add-properties-at stx path props)))


(define (syntax-add-properties-at stx path props)
  (define old-subform (syntax-ref stx path))
  (define new-subform
    (for/fold ([subform old-subform])
              ([(k v) (in-hash props)])
      (syntax-property subform k v)))
  (syntax-set stx path new-subform))


(module+ test
  (test-case "syntax-add-all-properties"
    (define stx #'(a (b c) d))
    (define props
      (syntax-property-bundle
       (syntax-property-entry empty-syntax-path 'size 3)
       (syntax-property-entry (syntax-path (list 0)) 'headphone-shaped? #false)
       (syntax-property-entry (syntax-path (list 1)) 'size 2)
       (syntax-property-entry (syntax-path (list 1 0)) 'headphone-shaped? #true)
       (syntax-property-entry (syntax-path (list 1 1)) 'headphone-shaped? #false)
       (syntax-property-entry (syntax-path (list 2)) 'headphone-shaped? #true)))

    (define stx-with-props (syntax-add-all-properties stx props))

    (check-equal? (syntax-property stx-with-props 'size) 3)
    (define/syntax-parse (a* bc* d*) stx-with-props)
    (check-equal? (syntax-property #'a* 'headphone-shaped?) #false)
    (check-equal? (syntax-property #'bc* 'size) 2)
    (check-equal? (syntax-property #'d* 'headphone-shaped?) #true)
    (define/syntax-parse (b* c*) #'bc*)
    (check-equal? (syntax-property #'b* 'headphone-shaped?) #true)
    (check-equal? (syntax-property #'c* 'headphone-shaped?) #false)))
