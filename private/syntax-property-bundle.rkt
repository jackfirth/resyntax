#lang racket/base


(require racket/contract/base)


(provide
 (struct-out syntax-property-entry)
 (contract-out
  [syntax-property-bundle (-> syntax-property-entry? ... syntax-property-bundle?)]
  [syntax-property-bundle? (-> any/c boolean?)]
  [syntax-property-bundle-as-map (-> syntax-property-bundle? immutable-sorted-map?)]
  [syntax-property-bundle-entries (-> syntax-property-bundle? (sequence/c syntax-property-entry?))]
  [syntax-property-bundle-get-property
   (->* (syntax-property-bundle? syntax-path? any/c) (failure-result/c) any/c)]
  [syntax-property-bundle-get-immediate-properties
   (-> syntax-property-bundle? syntax-path? immutable-hash?)]
  [syntax-property-bundle-get-all-properties
   (-> syntax-property-bundle? syntax-path? syntax-property-bundle?)]
  [sequence->syntax-property-bundle (-> (sequence/c syntax-property-entry?) syntax-property-bundle?)]
  [into-syntax-property-bundle (reducer/c syntax-property-entry? syntax-property-bundle?)]
  [property-hashes-into-syntax-property-bundle
   (reducer/c (entry/c syntax-path? immutable-hash?) syntax-property-bundle?)]
  [syntax-add-all-properties (-> syntax? syntax-property-bundle? syntax?)]
  [syntax-immediate-properties (->* (syntax?) (#:base-path syntax-path?) syntax-property-bundle?)]
  [syntax-all-properties (->* (syntax?) (#:base-path syntax-path?) syntax-property-bundle?)]))


(require guard
         racket/match
         racket/mutability
         racket/sequence
         racket/stream
         rebellion/base/range
         rebellion/collection/entry
         (except-in rebellion/collection/hash mutable-hash? immutable-hash?)
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


(define property-hashes-into-syntax-property-bundle
  (into-transduced
   (filtering-values (λ (prop-hash) (not (hash-empty? prop-hash))))
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
    (check-equal? actual expected))

  (test-case "property-hashes-into-syntax-property-bundle"
    (define prop-hashes
      (list (entry (syntax-path (list 0 1)) (hash 'foo 1))
                       (entry (syntax-path (list 0 2)) (hash 'bar 2 'baz 3))
                       (entry (syntax-path (list 0 3)) (hash))))

    (define actual (transduce prop-hashes #:into property-hashes-into-syntax-property-bundle))

    (define expected
      (syntax-property-bundle
       (syntax-property-entry (syntax-path (list 0 1)) 'foo 1)
       (syntax-property-entry (syntax-path (list 0 2)) 'bar 2)
       (syntax-property-entry (syntax-path (list 0 2)) 'baz 3)))
    (check-equal? actual expected)))


(define (syntax-property-bundle-get-property prop-bundle path key [failure-result #false])
  (define props-at-path (sorted-map-get (syntax-property-bundle-as-map prop-bundle) path (hash)))

  (define (fail)
    (raise-arguments-error
     'syntax-property-bundle-get-property
     "no property value for given key at given path"
     "path" path
     "property key" key
     "properties at path" props-at-path))

  (hash-ref props-at-path key (or failure-result fail)))


(module+ test
  (test-case "syntax-property-bundle-get-property"

    (test-case "bundle has entry for key and path"
      (define path (syntax-path (list 1 2 3)))
      (define props (syntax-property-bundle (syntax-property-entry path 'foo 42)))
      (check-equal? (syntax-property-bundle-get-property props path 'foo) 42))

    (test-case "empty bundle"
      (define path (syntax-path (list 1 2 3)))

      (define thrown
        (with-handlers ([any/c values])
          (syntax-property-bundle-get-property (syntax-property-bundle) path 'foo)
          #false))

      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"syntax-property-bundle-get-property:" (exn-message thrown))
      (check-regexp-match #rx"path:" (exn-message thrown))
      (check-regexp-match #rx"property key: 'foo" (exn-message thrown))
      (check-regexp-match #rx"properties at path: '#hash()" (exn-message thrown)))

    (test-case "empty bundle with failure value provided"
      (define path (syntax-path (list 1 2 3)))
      (define actual (syntax-property-bundle-get-property (syntax-property-bundle) path 'foo 42))
      (check-equal? actual 42))

    (test-case "empty bundle with failure thunk provided"
      (define path (syntax-path (list 1 2 3)))
      (define actual
        (syntax-property-bundle-get-property (syntax-property-bundle) path 'foo (λ () 42)))
      (check-equal? actual 42))))


(define (syntax-property-bundle-get-immediate-properties prop-bundle path)
  (sorted-map-get (syntax-property-bundle-as-map prop-bundle) path (hash)))


(module+ test
  (test-case "syntax-property-bundle-get-immediate-properties"

    (test-case "bundle has entry for path"
      (define path (syntax-path (list 1 2 3)))
      (define props
        (syntax-property-bundle
         (syntax-property-entry path 'foo 42)
         (syntax-property-entry path 'bar #true)
         (syntax-property-entry path 'baz #false)))

      (define actual (syntax-property-bundle-get-immediate-properties props path))

      (check-equal? actual (hash 'foo 42 'bar #true 'baz #false)))

    (test-case "empty bundle"
      (define path (syntax-path (list 1 2 3)))
      (define actual (syntax-property-bundle-get-immediate-properties (syntax-property-bundle) path))
      (check-equal? actual (hash)))))


(define/guard (syntax-property-bundle-get-all-properties prop-bundle path)
  (guard (nonempty-syntax-path? path) #:else prop-bundle)
  (define next-neighbor (syntax-path-next-neighbor path))
  (define path-range
    (if next-neighbor
        (closed-open-range path next-neighbor #:comparator syntax-path<=>)
        (at-least-range path)))
  (define submap (sorted-submap (syntax-property-bundle-as-map prop-bundle) path-range))
  (define new-map
    (transduce (in-sorted-map submap)
               (mapping-keys (λ (submap-path) (syntax-path-remove-prefix submap-path path)))
               #:into (into-sorted-map syntax-path<=>)))
  (constructor:syntax-property-bundle new-map))


(module+ test
  (test-case "syntax-property-bundle-get-all-properties"
    (void)))


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


(define (syntax-immediate-properties stx #:base-path [base-path empty-syntax-path])
  (define keys (syntax-property-symbol-keys stx))
  (transduce keys
             (mapping (λ (key)
                        (define value (syntax-property stx key))
                        (syntax-property-entry base-path key value)))
             #:into into-syntax-property-bundle))


(define (syntax-all-properties stx #:base-path [base-path empty-syntax-path])
  (transduce (in-syntax-paths stx #:base-path base-path)
             (append-mapping
              (λ (path)
                (define subform (syntax-ref stx (syntax-path-remove-prefix path base-path)))
                (define keys (syntax-property-symbol-keys subform))
                (for/list ([key (in-list keys)])
                  (define value (syntax-property subform key))
                  (syntax-property-entry path key value))))
             #:into into-syntax-property-bundle))


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


(module+ test
  (test-case "syntax-immediate-properties"

    (test-case "syntax with no properties"
      (define stx #'(a b c))
      (define props (syntax-immediate-properties stx))
      (check-true (syntax-property-bundle? props))
      (check-equal? (syntax-property-bundle-get-immediate-properties props empty-syntax-path)
                    (hash)))

    (test-case "syntax with single property"
      (define stx (syntax-property #'(a b c) 'foo 42))
      (define props (syntax-immediate-properties stx))
      (check-true (syntax-property-bundle? props))
      (define immediate (syntax-property-bundle-get-immediate-properties props empty-syntax-path))
      (check-equal? (hash-ref immediate 'foo) 42))

    (test-case "syntax with multiple properties"
      (define stx
        (syntax-property
         (syntax-property
          (syntax-property #'(a b c) 'foo 42)
          'bar #true)
         'baz "hello"))
      (define props (syntax-immediate-properties stx))
      (check-true (syntax-property-bundle? props))
      (define immediate (syntax-property-bundle-get-immediate-properties props empty-syntax-path))
      (check-equal? (hash-ref immediate 'foo) 42)
      (check-equal? (hash-ref immediate 'bar) #true)
      (check-equal? (hash-ref immediate 'baz) "hello"))

    (test-case "only extracts immediate properties, not from subforms"
      (define inner-stx (syntax-property #'x 'inner-prop 123))
      (define outer-stx
        (syntax-property
         (datum->syntax #f (list inner-stx #'y) #f)
         'outer-prop 456))
      (define props (syntax-immediate-properties outer-stx))
      (define immediate (syntax-property-bundle-get-immediate-properties props empty-syntax-path))
      (check-equal? (hash-ref immediate 'outer-prop #false) 456)
      (check-equal? (hash-ref immediate 'inner-prop #false) #false))))


(module+ test
  (test-case "syntax-all-properties"

    (test-case "syntax with no properties"
      (define stx #'(a b c))
      (define props (syntax-all-properties stx))
      (check-true (syntax-property-bundle? props))
      (check-equal? (sequence-length (syntax-property-bundle-entries props)) 0))

    (test-case "syntax with property only on root"
      (define stx (syntax-property #'(a b c) 'root-prop 42))
      (define props (syntax-all-properties stx))
      (check-true (syntax-property-bundle? props))
      (define root-props (syntax-property-bundle-get-immediate-properties props empty-syntax-path))
      (check-equal? (hash-ref root-props 'root-prop) 42))

    (test-case "syntax with properties on multiple subforms"
      (define a-stx (syntax-property #'a 'a-prop 1))
      (define b-stx (syntax-property #'b 'b-prop 2))
      (define c-stx (syntax-property #'c 'c-prop 3))
      (define stx (datum->syntax #f (list a-stx b-stx c-stx) #f))
      (define stx-with-root (syntax-property stx 'root-prop 0))
      (define props (syntax-all-properties stx-with-root))
      (check-true (syntax-property-bundle? props))
      
      (define root-props (syntax-property-bundle-get-immediate-properties props empty-syntax-path))
      (check-equal? (hash-ref root-props 'root-prop) 0)
      
      (define a-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 0))))
      (check-equal? (hash-ref a-props 'a-prop) 1)
      
      (define b-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 1))))
      (check-equal? (hash-ref b-props 'b-prop) 2)
      
      (define c-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 2))))
      (check-equal? (hash-ref c-props 'c-prop) 3))

    (test-case "nested syntax with properties"
      (define inner-b (syntax-property #'b 'inner-prop 42))
      (define inner-c (syntax-property #'c 'inner-prop 99))
      (define inner-list (datum->syntax #f (list inner-b inner-c) #f))
      (define inner-with-prop (syntax-property inner-list 'list-prop 10))
      (define outer (datum->syntax #f (list #'a inner-with-prop #'d) #f))
      (define props (syntax-all-properties outer))
      
      (define inner-list-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 1))))
      (check-equal? (hash-ref inner-list-props 'list-prop) 10)
      
      (define b-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 1 0))))
      (check-equal? (hash-ref b-props 'inner-prop) 42)
      
      (define c-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 1 1))))
      (check-equal? (hash-ref c-props 'inner-prop) 99))

    (test-case "with base-path parameter"
      (define stx (syntax-property #'(a b c) 'root-prop 42))
      (define base (syntax-path (list 5 10)))
      (define props (syntax-immediate-properties stx #:base-path base))
      (check-true (syntax-property-bundle? props))
      (define base-props (syntax-property-bundle-get-immediate-properties props base))
      (check-equal? (hash-ref base-props 'root-prop) 42))

    (test-case "syntax-all-properties with base-path"
      (define a-stx (syntax-property #'a 'a-prop 1))
      (define b-stx (syntax-property #'b 'b-prop 2))
      (define stx (datum->syntax #f (list a-stx b-stx) #f))
      (define base (syntax-path (list 3 7)))
      (define props (syntax-all-properties stx #:base-path base))
      
      (define a-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 3 7 0))))
      (check-equal? (hash-ref a-props 'a-prop) 1)
      
      (define b-props (syntax-property-bundle-get-immediate-properties props (syntax-path (list 3 7 1))))
      (check-equal? (hash-ref b-props 'b-prop) 2))))
