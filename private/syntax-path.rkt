#lang racket/base


(require racket/contract/base)


(provide
 (struct-out vector-element-syntax)
 (struct-out hash-value-syntax)
 (struct-out prefab-field-syntax)
 (struct-out tail-syntax)
 (contract-out
  [syntax-path? (-> any/c boolean?)]
  [syntax-path<=> (comparator/c syntax-path?)]
  [nonempty-syntax-path? (-> any/c boolean?)]
  [empty-syntax-path syntax-path?]
  [syntax-path (-> (sequence/c syntax-path-element?) syntax-path?)]
  [syntax-path-elements (-> syntax-path? (treelist/c syntax-path-element?))]
  [syntax-path-element? (-> any/c boolean?)]
  [syntax-path-parent (-> nonempty-syntax-path? syntax-path?)]
  [syntax-path-last-element (-> nonempty-syntax-path? syntax-path-element?)]
  [syntax-path-add (-> syntax-path? syntax-path-element? syntax-path?)]
  [syntax-path-neighbors? (-> syntax-path? syntax-path? boolean?)]
  [syntax-ref (-> syntax? syntax-path? syntax?)]
  [syntax-label-paths (-> syntax? symbol? syntax?)]
  [box-element-syntax syntax-path-element?]))


(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/sequence
                     racket/syntax)
         data/order
         guard
         racket/sequence
         racket/struct
         racket/treelist
         racket/list
         racket/match
         rebellion/base/comparator
         rebellion/type/singleton
         resyntax/private/matching-comparator
         syntax/parse/define)


(module+ test
  (require (submod "..")
           racket/syntax
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (syntax-path-element? v)
  (or (exact-nonnegative-integer? v)
      (tail-syntax? v)
      (box-element-syntax? v)
      (vector-element-syntax? v)
      (hash-value-syntax? v)
      (prefab-field-syntax? v)))


(define-singleton-type box-element-syntax)


(struct tail-syntax (index)
  #:transparent
  #:sealed
  #:guard (struct-guard/c exact-nonnegative-integer?))


(struct vector-element-syntax (index)
  #:transparent
  #:sealed
  #:guard (struct-guard/c exact-nonnegative-integer?))


(struct hash-value-syntax (key-datum)
  #:transparent
  #:sealed)


(struct prefab-field-syntax (index)
  #:transparent
  #:sealed
  #:guard (struct-guard/c exact-nonnegative-integer?))


(struct syntax-path (elements)
  #:transparent
  #:sealed
  #:guard (λ (elements _) (sequence->treelist elements)))


(define empty-syntax-path (syntax-path (treelist)))


(define (nonempty-syntax-path? v)
  (and (syntax-path? v) (not (treelist-empty? (syntax-path-elements v)))))


(module+ test
  (test-case "nonempty-syntax-path?"
    (check-false (nonempty-syntax-path? empty-syntax-path))
    (check-true (nonempty-syntax-path? (syntax-path (list 0))))
    (check-false (nonempty-syntax-path? 42))))


(define (syntax-path-add path element)
  (syntax-path (treelist-add (syntax-path-elements path) element)))


(module+ test
  (test-case "syntax-path-add"
    (check-equal? (syntax-path-add empty-syntax-path 0) (syntax-path (list 0)))
    (check-equal? (syntax-path-add (syntax-path (list 0)) box-element-syntax)
                  (syntax-path (list 0 box-element-syntax)))))


(define (syntax-path-parent path)
  (syntax-path (treelist-drop-right (syntax-path-elements path) 1)))


(module+ test
  (test-case "syntax-path-parent"
    (check-equal? (syntax-path-parent (syntax-path (list 0))) empty-syntax-path)
    (check-equal? (syntax-path-parent (syntax-path (list 0 box-element-syntax)))
                  (syntax-path (list 0)))
    (check-exn exn:fail:contract? (λ () (syntax-path-parent empty-syntax-path)))
    (check-exn #rx"expected: nonempty-syntax-path?" (λ () (syntax-path-parent empty-syntax-path)))))


(define (syntax-path-last-element path)
  (treelist-last (syntax-path-elements path)))


(module+ test
  (test-case "syntax-path-last-element"
    (check-equal? (syntax-path-last-element (syntax-path (list 0))) 0)
    (check-equal? (syntax-path-last-element (syntax-path (list 0 box-element-syntax)))
                  box-element-syntax)
    (check-exn exn:fail:contract? (λ () (syntax-path-last-element empty-syntax-path)))
    (check-exn #rx"expected: nonempty-syntax-path?"
               (λ () (syntax-path-last-element empty-syntax-path)))))


(define (syntax-path-neighbors? leading-path trailing-path)
  (and (nonempty-syntax-path? leading-path)
       (nonempty-syntax-path? trailing-path)
       (equal? (syntax-path-parent leading-path) (syntax-path-parent trailing-path))
       (syntax-path-element-neighbors?
        (syntax-path-last-element leading-path) (syntax-path-last-element trailing-path))))


(define (syntax-path-element-neighbors? leading trailing)
  (match (list leading trailing)
    [(list (? exact-nonnegative-integer? i) (? exact-nonnegative-integer? j)) (equal? i (sub1 j))]
    [(list (vector-element-syntax i) (vector-element-syntax j)) (equal? i (sub1 j))]
    [(list (prefab-field-syntax i) (prefab-field-syntax j)) (equal? i (sub1 j))]
    [(list (? exact-nonnegative-integer? i) (tail-syntax j)) (equal? i (sub1 j))]
    [(list _ _) #false]))


(define (syntax-ref init-stx path)
  (define result
    (for/fold ([stx init-stx])
              ([element (in-treelist (syntax-path-elements path))])
      (define unwrapped
        ; It's only *not* syntax in the case where `tail-syntax` was used to pick out a trailing
        ; list of subforms of a form. These sorts of syntax objects get created by #%app macro
        ; insertion, which is how I discovered this check was necessary.
        (if (syntax? stx)
            (syntax-e stx)
            stx))
      (match element
        [(? exact-nonnegative-integer? i)
         (unless (possibly-improper-list-of-minimum-size? unwrapped (add1 i))
           (raise-arguments-error 'syntax-ref
                                  "syntax path is inconsistent with the syntax's shape"
                                  "syntax" init-stx
                                  "path" path
                                  "malformed subform" stx
                                  "path element" element))
         (list-ref unwrapped i)]
        [(tail-syntax i) (drop unwrapped i)]
        [(vector-element-syntax i) (vector-ref unwrapped i)]
        [(== box-element-syntax) (unbox unwrapped)]
        [(hash-value-syntax key) (hash-ref unwrapped key)]
        [(prefab-field-syntax i) (prefab-struct-ref unwrapped i)])))
  (when (or (pair? result) (empty? result))
    (raise-arguments-error 'syntax-ref
                           "syntax path refers to a non-syntax component"
                           "syntax" init-stx
                           "path" path
                           "component" result))
  result)


(module+ test
  (test-case "syntax-ref"

    (test-case "empty path"
      (define stx #'a)
      (define actual (syntax-ref stx empty-syntax-path))
      (check-equal? actual stx))

    (test-case "list element path"
      (define stx #'(a b c))
      (define actual (syntax-ref stx (syntax-path (list 1))))
      (check-equal? (syntax->datum actual) 'b))

    (test-case "tail syntax path"
      (define stx #'(a . (b c)))
      (define actual (syntax-ref stx (syntax-path (list (tail-syntax 1)))))
      (check-equal? (syntax->datum actual) '(b c)))

    (test-case "tail syntax path of flat syntax list"
      (define stx #'(a b c))
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (syntax-ref stx (syntax-path (list (tail-syntax 1))))
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"syntax-ref:" (exn-message thrown))
      (check-regexp-match #rx"path refers to a non-syntax component" (exn-message thrown)))

    (test-case "vector element path"
      (define stx #'#[a b c])
      (define actual (syntax-ref stx (syntax-path (list (vector-element-syntax 1)))))
      (check-equal? (syntax->datum actual) 'b))

    (test-case "box element path"
      (define stx #'#&a)
      (define actual (syntax-ref stx (syntax-path (list box-element-syntax))))
      (check-equal? (syntax->datum actual) 'a))

    (test-case "hash value path"
      (define stx #'#hash((a . 1) (b . 2) (c . 3)))
      (define actual (syntax-ref stx (syntax-path (list (hash-value-syntax 'b)))))
      (check-equal? (syntax->datum actual) 2))

    (test-case "nested list path"
      (define stx #'(a b c (m (FOO x y z) n)))
      (define actual (syntax-ref stx (syntax-path (list 3 1 0))))
      (check-equal? (syntax->datum actual) 'FOO))

    (test-case "list element after tail syntax path"
      (define stx #'(a b . (c FOO e)))
      (define actual (syntax-ref stx (syntax-path (list (tail-syntax 2) 1))))
      (check-equal? (syntax->datum actual) 'FOO))

    (test-case "list element after tail syntax path in flat syntax"
      (define stx #'(a b c FOO e))
      (define actual (syntax-ref stx (syntax-path (list (tail-syntax 2) 1))))
      (check-equal? (syntax->datum actual) 'FOO))

    (test-case "list element on syntax that's too short"
      (define stx #'(a b c))
      (define thrown
        (with-handlers ([(λ (_) #true) values])
          (syntax-ref stx (syntax-path (list 10)))
          #false))
      (check-pred exn:fail:contract? thrown)
      (check-regexp-match #rx"syntax-ref:" (exn-message thrown))
      (check-regexp-match #rx"path is inconsistent" (exn-message thrown)))))


(define (syntax-label-paths stx property-name)
  (let loop ([stx stx] [path (syntax-path '())])
    (define datum-with-children-labeled
      (match (syntax-e stx)
        [(list children ...)
         (for/list ([child (in-list children)]
                    [i (in-naturals)])
           (loop child (syntax-path-add path i)))]
        [(list-rest children ... tail-child)
         #:when (not (empty? children))
         (append (for/list ([child (in-list children)]
                            [i (in-naturals)])
                   (loop child (syntax-path-add path i)))
                 (loop tail-child (syntax-path-add path (tail-syntax (length children)))))]
        [(vector children ...)
         (for/vector ([child (in-list children)]
                      [i (in-naturals)])
           (loop child (syntax-path-add path (vector-element-syntax i))))]
        [(box child) (box-immutable (loop child (syntax-path-add path box-element-syntax)))]
        [(? hash? ht)
         (define (label-entry key value-child)
           (values key (loop value-child (syntax-path-add path (hash-value-syntax key)))))
         (hash-map/copy ht label-entry)]
        [(? prefab-struct? s)
         (define key (prefab-struct-key s))
         (define labeled-children
           (for/list ([child (in-list (struct->list s))]
                      [i (in-naturals)])
             (loop child (syntax-path-add path (prefab-field-syntax i)))))
         (apply make-prefab-struct key labeled-children)]
        [_ stx]))
    (define stx-with-children-labeled (datum->syntax stx datum-with-children-labeled stx stx))
    (syntax-property stx-with-children-labeled property-name path)))


(module+ test
  (test-case "syntax-label-paths"
    (define stx #'(foo (a b . c) bar (baz) #(x y) #&z #s(point n m)))
    (define labeled (syntax-label-paths stx 'path))
    (check-equal? (syntax->datum labeled) (syntax->datum stx))
    (define/with-syntax (foo* (a* b* . c*) bar* (baz*) #(x* y*) #&z* #s(point n* m*)) labeled)
    (check-equal? (syntax-property #'foo* 'path) (syntax-path (treelist 0)))
    (check-equal? (syntax-property #'a* 'path) (syntax-path (treelist 1 0)))
    (check-equal? (syntax-property #'b* 'path) (syntax-path (treelist 1 1)))
    (check-equal? (syntax-property #'c* 'path) (syntax-path (treelist 1 (tail-syntax 2))))
    (check-equal? (syntax-property #'bar* 'path) (syntax-path (treelist 2)))
    (check-equal? (syntax-property #'baz* 'path) (syntax-path (treelist 3 0)))
    (check-equal? (syntax-property #'x* 'path) (syntax-path (treelist 4 (vector-element-syntax 0))))
    (check-equal? (syntax-property #'y* 'path) (syntax-path (treelist 4 (vector-element-syntax 1))))
    (check-equal? (syntax-property #'z* 'path) (syntax-path (treelist 5 box-element-syntax)))
    (check-equal? (syntax-property #'n* 'path) (syntax-path (treelist 6 (prefab-field-syntax 0))))
    (check-equal? (syntax-property #'m* 'path) (syntax-path (treelist 6 (prefab-field-syntax 1))))
    (for ([id (in-syntax #'(foo* a* b* c* bar* baz* x* y* z* n* m*))])
      (define path (syntax-property id 'path))
      (check-equal? (syntax->datum (syntax-ref stx path)) (syntax->datum id)))))


(define (prefab-struct? v)
  (and (prefab-struct-key v) #true))


(define (prefab-struct-ref s i)
  (unless (prefab-struct? s)
    (raise-argument-error 'prefab-struct-ref "prefab-struct?" s))
  (list-ref (struct->list s) i))



(define datum<=>
  (make-comparator
   (λ (left right)
     (match (datum-order left right)
       ['= equivalent]
       ['> greater]
       ['< lesser]))))


(define syntax-path-element<=>
  (matching-comparator
   [(? exact-nonnegative-integer? i) #:compare i]
   [(tail-syntax i) #:compare i]
   [(vector-element-syntax i) #:compare i]
   [(hash-value-syntax key) #:compare key datum<=>]
   [(== box-element-syntax)]
   [(prefab-field-syntax i) #:compare i]))


(module+ test
  (test-case "syntax-path-element<=>"
    (define unsorted
      (list 2
            (tail-syntax 1)
            box-element-syntax
            1
            3
            (hash-value-syntax 'foo)
            (tail-syntax 4)
            (vector-element-syntax 5)))

    (define sorted
      (sort unsorted (λ (a b) (compare-infix syntax-path-element<=>  a < b))))

    (define expected
      (list 1
            2
            3
            (tail-syntax 1)
            (tail-syntax 4)
            (vector-element-syntax 5)
            (hash-value-syntax 'foo)
            box-element-syntax))
    (check-equal? sorted expected)))


(define syntax-path<=>
  (comparator-map (lexicographic-comparator syntax-path-element<=>) syntax-path-elements
                  #:name 'syntax-path<=>))


(define (possibly-improper-list-of-minimum-size? v size)
  (or (zero? size)
      (and (pair? v) (possibly-improper-list-of-minimum-size? (cdr v) (sub1 size)))))


(module+ test
  (test-case "possibly-improper-list-of-minimum-size?"
    (check-true (possibly-improper-list-of-minimum-size? '(a b c d e) 2))
    (check-false (possibly-improper-list-of-minimum-size? '(a b c d e) 8))
    (check-true (possibly-improper-list-of-minimum-size? '(a b c d . e) 2))
    (check-false (possibly-improper-list-of-minimum-size? '(a b c d . e) 8))
    (check-true (possibly-improper-list-of-minimum-size? 'a 0))
    (check-false (possibly-improper-list-of-minimum-size? 'a 1))))
