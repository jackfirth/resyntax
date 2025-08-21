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
  [empty-syntax-path? (-> any/c boolean?)]
  [nonempty-syntax-path? (-> any/c boolean?)]
  [proper-syntax-path? (-> any/c boolean?)]
  [empty-syntax-path syntax-path?]
  [syntax-path (-> (sequence/c syntax-path-element?) syntax-path?)]
  [syntax-path-elements (-> syntax-path? (treelist/c syntax-path-element?))]
  [syntax-path-element? (-> any/c boolean?)]
  [syntax-path-parent (-> nonempty-syntax-path? syntax-path?)]
  [syntax-path-next-neighbor (-> syntax-path? (or/c syntax-path? #false))]
  [syntax-path-last-element (-> nonempty-syntax-path? syntax-path-element?)]
  [syntax-path-add (-> syntax-path? syntax-path-element? syntax-path?)]
  [syntax-path-remove-prefix (-> syntax-path? syntax-path? syntax-path?)]
  [syntax-path-neighbors? (-> syntax-path? syntax-path? boolean?)]
  [syntax-ref (-> syntax? syntax-path? syntax?)]
  [syntax-set (-> syntax? syntax-path? syntax? syntax?)]
  [syntax-remove-splice
   (-> syntax? (and/c proper-syntax-path? nonempty-syntax-path?) exact-nonnegative-integer? syntax?)]
  [syntax-insert-splice
   (-> syntax? (and/c proper-syntax-path? nonempty-syntax-path?) (sequence/c syntax?) syntax?)]
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
         racket/vector
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


(define (empty-syntax-path? v)
  (and (syntax-path? v) (treelist-empty? (syntax-path-elements v))))


(module+ test
  (test-case "empty-syntax-path?"
    (check-true (empty-syntax-path? empty-syntax-path))
    (check-false (empty-syntax-path? (syntax-path (list 0))))
    (check-false (empty-syntax-path? 42))))


(define (nonempty-syntax-path? v)
  (and (syntax-path? v) (not (treelist-empty? (syntax-path-elements v)))))


(module+ test
  (test-case "nonempty-syntax-path?"
    (check-false (nonempty-syntax-path? empty-syntax-path))
    (check-true (nonempty-syntax-path? (syntax-path (list 0))))
    (check-false (nonempty-syntax-path? 42))))


(define (proper-syntax-path? v)
  (and (syntax-path? v)
       (for/and ([elem (in-treelist (syntax-path-elements v))])
         (exact-nonnegative-integer? elem))))


; TODO: add tests for proper-syntax-path?


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


(define/guard (syntax-path-next-neighbor path)
  (define elements (syntax-path-elements path))
  (guard (not (treelist-empty? elements)) #:else #false)
  (define parent-elems (treelist-drop-right elements 1))
  (match (treelist-last elements)
    [(? exact-nonnegative-integer? i)
     (syntax-path (treelist-add parent-elems (add1 i)))]
    [_ #false]))


(module+ test
  (test-case "syntax-path-next-neighbor"

    (test-case "empty path"
      (check-false (syntax-path-next-neighbor empty-syntax-path)))

    (test-case "first child"
      (define path (syntax-path (list 0)))
      (define expected (syntax-path (list 1)))
      (check-equal? (syntax-path-next-neighbor path) expected))

    (test-case "nth child"
      (define path (syntax-path (list 42)))
      (define expected (syntax-path (list 43)))
      (check-equal? (syntax-path-next-neighbor path) expected))

    (test-case "nested list child"
      (define path (syntax-path (list 1 2 3 42)))
      (define expected (syntax-path (list 1 2 3 43)))
      (check-equal? (syntax-path-next-neighbor path) expected))

    ; TODO: handle non-list element children
    (void)))


(define (syntax-path-remove-prefix path prefix)
  (define elems (syntax-path-elements path))
  (define prefix-elems (syntax-path-elements prefix))
  (unless (>= (treelist-length elems) (treelist-length prefix-elems))
    (raise-arguments-error
     'syntax-path-remove-prefix "path is shorter than prefix" "path" path "prefix" prefix))
  (define elems-up-to (treelist-take elems (treelist-length prefix-elems)))
  (unless (equal? elems-up-to prefix-elems)
    (raise-arguments-error
     'syntax-path-remove-prefix "path does not start with given prefix" "path" path "prefix" prefix))
  (syntax-path (treelist-drop elems (treelist-length prefix-elems))))


(module+ test
  (test-case "syntax-path-remove-prefix"

    (test-case "remove empty"
      (define path (syntax-path (list 1 2 3)))
      (check-equal? (syntax-path-remove-prefix path empty-syntax-path) path))

    (test-case "remove one elem"
      (define path (syntax-path (list 1 2 3)))
      (define prefix (syntax-path (list 1)))
      (define expected (syntax-path (list 2 3)))
      (check-equal? (syntax-path-remove-prefix path prefix) expected))

    (test-case "remove multiple elems"
      (define path (syntax-path (list 1 2 3 4 5)))
      (define prefix (syntax-path (list 1 2 3)))
      (define expected (syntax-path (list 4 5)))
      (check-equal? (syntax-path-remove-prefix path prefix) expected))))


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


(define (syntax-set init-stx path new-subform)
  (let loop ([stx init-stx] [elements (syntax-path-elements path)])
    (guarded-block
      (guard (not (treelist-empty? elements)) #:else new-subform)
      (define next-element (treelist-first elements))
      (define remaining-elements (treelist-rest elements))
      (define unwrapped
        ; It's only *not* syntax in the case where `tail-syntax` was used to pick out a trailing
        ; list of subforms of a form. These sorts of syntax objects get created by #%app macro
        ; insertion, which is how I discovered this check was necessary.
        (if (syntax? stx)
            (syntax-e stx)
            stx))
      (match next-element
        [(? exact-nonnegative-integer? i)
         (define updated-child (loop (list-ref unwrapped i) remaining-elements))
         (define updated-datum (improper-list-set unwrapped i updated-child))
         (if (syntax? stx)
             (datum->syntax stx updated-datum stx stx)
             updated-datum)]
        [(tail-syntax i)
         (define tail-part (drop unwrapped i))
         (define updated-tail (loop tail-part remaining-elements))
         (define updated-datum (append (take unwrapped i) updated-tail))
         (datum->syntax stx updated-datum stx stx)]
        [(vector-element-syntax i)
         (define updated-child (loop (vector-ref unwrapped i) remaining-elements))
         (define updated-vector (vector-copy unwrapped))
         (vector-set! updated-vector i updated-child)
         (datum->syntax stx updated-vector stx stx)]
        [(== box-element-syntax)
         (define updated-child (loop (unbox unwrapped) remaining-elements))
         (define updated-datum (box-immutable updated-child))
         (datum->syntax stx updated-datum stx stx)]
        [(hash-value-syntax key)
         (define updated-child (loop (hash-ref unwrapped key) remaining-elements))
         (define updated-datum (hash-set unwrapped key updated-child))
         (datum->syntax stx updated-datum stx stx)]
        [(prefab-field-syntax i)
         (define updated-child (loop (prefab-struct-ref unwrapped i) remaining-elements))
         (define key (prefab-struct-key unwrapped))
         (define fields (struct->list unwrapped))
         (define updated-fields (list-set fields i updated-child))
         (define updated-datum (apply make-prefab-struct key updated-fields))
         (datum->syntax stx updated-datum stx stx)]))))


(module+ test
  (test-case "syntax-set"

    (define new-subform #'FOO)

    (test-case "empty path"
      (define stx #'a)
      (define actual (syntax-set stx empty-syntax-path new-subform))
      (check-equal? actual new-subform))

    (test-case "list element path"
      (define stx #'(a b c))
      (define actual (syntax-set stx (syntax-path (list 1)) new-subform))
      (check-equal? (syntax->datum actual) '(a FOO c)))

    (test-case "tail syntax path"
      (define stx #'(a . (b c)))
      (define actual (syntax-set stx (syntax-path (list (tail-syntax 1))) #'(FOO bar)))
      (check-equal? (syntax->datum actual) '(a FOO bar)))

    (test-case "tail syntax path of flat syntax list"
      (define stx #'(a b c))
      (define actual (syntax-set stx (syntax-path (list (tail-syntax 1))) #'(FOO bar)))
      (check-equal? (syntax->datum actual) '(a FOO bar)))

    (test-case "vector element path"
      (define stx #'#[a b c])
      (define actual (syntax-set stx (syntax-path (list (vector-element-syntax 1))) new-subform))
      (check-equal? (syntax->datum actual) '#[a FOO c]))

    (test-case "box element path"
      (define stx #'#&a)
      (define actual (syntax-set stx (syntax-path (list box-element-syntax)) new-subform))
      (check-equal? (syntax->datum actual) '#&FOO))

    (test-case "hash value path"
      (define stx #'#hash((a . 1) (b . 2) (c . 3)))
      (define actual (syntax-set stx (syntax-path (list (hash-value-syntax 'b))) new-subform))
      (check-equal? (syntax->datum actual) '#hash((a . 1) (b . FOO) (c . 3))))

    (test-case "prefab field path"
      (define stx #'#s(point 1 2))
      (define actual (syntax-set stx (syntax-path (list (prefab-field-syntax 0))) new-subform))
      (check-equal? (syntax->datum actual) '#s(point FOO 2)))

    (test-case "nested list path"
      (define stx #'(a b c (m (OLD x y z) n)))
      (define actual (syntax-set stx (syntax-path (list 3 1 0)) new-subform))
      (check-equal? (syntax->datum actual) '(a b c (m (FOO x y z) n))))

    (test-case "list element after tail syntax path"
      (define stx #'(a b . (c OLD e)))
      (define actual (syntax-set stx (syntax-path (list (tail-syntax 2) 1)) new-subform))
      (check-equal? (syntax->datum actual) '(a b c FOO e)))

    (test-case "list element after tail syntax path in flat syntax"
      (define stx #'(a b c OLD e))
      (define actual (syntax-set stx (syntax-path (list (tail-syntax 2) 1)) new-subform))
      (check-equal? (syntax->datum actual) '(a b c FOO e)))))


(define/guard (syntax-remove-splice stx path children-count)
  (guard (positive? children-count) #:else stx)
  (define parent (syntax-ref stx (syntax-path-parent path)))
  (define updated
    (list-remove-splice (syntax->list parent) (syntax-path-last-element path) children-count))
  (define new-parent (datum->syntax parent updated parent parent))
  (syntax-set stx (syntax-path-parent path) new-parent))


(module+ test
  (test-case "syntax-remove-splice"
    (test-case "empty splice"
      (define stx #'(a b c))
      (define actual (syntax-remove-splice stx (syntax-path (list 1)) 0))
      (check-eq? actual stx))

    (test-case "singleton splice"
      (define stx #'(a b c))
      (define actual (syntax-remove-splice stx (syntax-path (list 1)) 1))
      (check-equal? (syntax->datum actual) '(a c)))

    (test-case "remove multiple elements"
      (define stx #'(a b c d e))
      (define actual (syntax-remove-splice stx (syntax-path (list 1)) 2))
      (check-equal? (syntax->datum actual) '(a d e)))

    (test-case "remove from start"
      (define stx #'(a b c d))
      (define actual (syntax-remove-splice stx (syntax-path (list 0)) 2))
      (check-equal? (syntax->datum actual) '(c d)))

    (test-case "remove from end"
      (define stx #'(a b c d))
      (define actual (syntax-remove-splice stx (syntax-path (list 2)) 2))
      (check-equal? (syntax->datum actual) '(a b)))

    (test-case "remove more than available - should error"
      (define stx #'(a b c))
      (check-exn exn:fail?
                 (λ () (syntax-remove-splice stx (syntax-path (list 1)) 10))))

    (test-case "nested list removal"
      (define stx #'(a (x y z) b))
      (define actual (syntax-remove-splice stx (syntax-path (list 1 1)) 1))
      (check-equal? (syntax->datum actual) '(a (x z) b)))

    (test-case "remove from empty list - should error"
      (define stx #'())
      (check-exn exn:fail?
                 (λ () (syntax-remove-splice stx (syntax-path (list 0)) 1))))

    (test-case "error on empty path with non-zero count"
      (define stx #'(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-remove-splice stx empty-syntax-path 1))))

    (test-case "error on non-list target"
      (define stx #'#(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-remove-splice stx (syntax-path (list 0)) 1))))))


(define/guard (syntax-insert-splice stx path new-children)
  (guard (not (empty? new-children)) #:else stx)
  (define parent (syntax-ref stx (syntax-path-parent path)))
  (define updated
    (list-insert-splice (syntax->list parent) (syntax-path-last-element path) new-children))
  (define new-parent (datum->syntax parent updated parent parent))
  (syntax-set stx (syntax-path-parent path) new-parent))


(module+ test
  (test-case "syntax-insert-splice"
    (test-case "empty splice"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 1)) '()))
      (check-eq? actual stx))

    (test-case "singleton splice"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 1)) (list #'foo)))
      (check-equal? (syntax->datum actual) '(a foo b c)))

    (test-case "multiple element splice"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 1)) (list #'foo #'bar)))
      (check-equal? (syntax->datum actual) '(a foo bar b c)))

    (test-case "insert at start"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 0)) (list #'x #'y)))
      (check-equal? (syntax->datum actual) '(x y a b c)))

    (test-case "insert at end"
      (define stx #'(a b c))
      (define actual (syntax-insert-splice stx (syntax-path (list 3)) (list #'x #'y)))
      (check-equal? (syntax->datum actual) '(a b c x y)))

    (test-case "insert beyond end"
      (define stx #'(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-insert-splice stx (syntax-path (list 10)) (list #'x #'y)))))

    (test-case "nested list insertion"
      (define stx #'(a (x z) b))
      (define actual (syntax-insert-splice stx (syntax-path (list 1 1)) (list #'y)))
      (check-equal? (syntax->datum actual) '(a (x y z) b)))

    (test-case "insert into empty list"
      (define stx #'())
      (define actual (syntax-insert-splice stx (syntax-path (list 0)) (list #'x)))
      (check-equal? (syntax->datum actual) '(x)))

    (test-case "error on empty path"
      (define stx #'(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-insert-splice stx empty-syntax-path (list #'x)))))

    (test-case "error on non-list target"
      (define stx #'#(a b c))
      (check-exn exn:fail:contract?
                 (λ () (syntax-insert-splice stx (syntax-path (list 0)) (list #'x)))))))


(define (list-remove-splice lst i splice-length)
  (append (take lst i) (drop lst (+ i splice-length))))


(module+ test
  (test-case "list-remove-splice"
    (test-case "remove from middle"
      (define lst '(a b c d e))
      (define actual (list-remove-splice lst 2 2))
      (check-equal? actual '(a b e)))
    
    (test-case "remove from beginning"
      (define lst '(a b c d))
      (define actual (list-remove-splice lst 0 2))
      (check-equal? actual '(c d)))
    
    (test-case "remove from end"
      (define lst '(a b c d))
      (define actual (list-remove-splice lst 2 2))
      (check-equal? actual '(a b)))
    
    (test-case "remove single element"
      (define lst '(a b c))
      (define actual (list-remove-splice lst 1 1))
      (check-equal? actual '(a c)))
    
    (test-case "remove zero elements"
      (define lst '(a b c))
      (define actual (list-remove-splice lst 1 0))
      (check-equal? actual '(a b c)))
    
    (test-case "remove all elements"
      (define lst '(a b c))
      (define actual (list-remove-splice lst 0 3))
      (check-equal? actual '()))
    
    (test-case "remove from single-element list"
      (define lst '(a))
      (define actual (list-remove-splice lst 0 1))
      (check-equal? actual '()))
    
    (test-case "remove from empty list"
      (define lst '())
      (define actual (list-remove-splice lst 0 0))
      (check-equal? actual '()))
    
    (test-case "error on index out of bounds"
      (define lst '(a b c))
      (check-exn exn:fail:contract?
                 (λ () (list-remove-splice lst 5 1))))
    
    (test-case "error on splice length too large"
      (define lst '(a b c))
      (check-exn exn:fail:contract?
                 (λ () (list-remove-splice lst 1 5))))))


(define (list-insert-splice lst i splice)
  (append (take lst i) splice (drop lst i)))


(module+ test
  (test-case "list-insert-splice"
    (test-case "insert in middle"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 1 '(x y)))
      (check-equal? actual '(a x y b c)))
    
    (test-case "insert at beginning"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 0 '(x y)))
      (check-equal? actual '(x y a b c)))
    
    (test-case "insert at end"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 3 '(x y)))
      (check-equal? actual '(a b c x y)))
    
    (test-case "insert single element"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 1 '(x)))
      (check-equal? actual '(a x b c)))
    
    (test-case "insert empty splice"
      (define lst '(a b c))
      (define actual (list-insert-splice lst 1 '()))
      (check-equal? actual '(a b c)))
    
    (test-case "insert into empty list"
      (define lst '())
      (define actual (list-insert-splice lst 0 '(x y)))
      (check-equal? actual '(x y)))
    
    (test-case "insert into single-element list"
      (define lst '(a))
      (define actual (list-insert-splice lst 0 '(x y)))
      (check-equal? actual '(x y a)))
    
    (test-case "insert after single element"
      (define lst '(a))
      (define actual (list-insert-splice lst 1 '(x y)))
      (check-equal? actual '(a x y)))
    
    (test-case "error on index out of bounds"
      (define lst '(a b c))
      (check-exn exn:fail:contract?
                 (λ () (list-insert-splice lst 5 '(x y)))))))


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


(define (improper-list-set lst i v)
  (cond
    [(positive? i) (cons (car lst) (improper-list-set (cdr lst) (sub1 i) v))]
    [(pair? lst) (cons v (cdr lst))]
    [else v]))


(module+ test
  (test-case "improper-list-set"
    (check-equal? (improper-list-set '(a b c) 0 'FOO) '(FOO b c))
    (check-equal? (improper-list-set '(a b . c) 0 'FOO) '(FOO b . c))
    (check-equal? (improper-list-set '(a b c) 2 'FOO) '(a b FOO))
    (check-equal? (improper-list-set '(a b . c) 2 'FOO) '(a b . FOO))))
