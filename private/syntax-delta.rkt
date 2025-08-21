#lang racket/base


(require racket/contract/base)


(provide
 (struct-out splice-replacement)
 (struct-out new-syntax)
 (struct-out copied-syntax)
 (contract-out
  [syntax-delta? (-> any/c boolean?)]
  [syntax-delta (-> (sequence/c splice-replacement?) syntax-delta?)]
  [added-syntax? (-> any/c boolean?)]))


(require racket/match
         racket/sequence
         racket/treelist
         rebellion/base/comparator
         rebellion/collection/entry
         rebellion/collection/sorted-map
         rebellion/streaming/transducer
         resyntax/private/syntax-path)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct added-syntax () #:transparent)


(struct new-syntax added-syntax (object)
  #:transparent
  #:sealed
  #:guard (struct-guard/c syntax?))


(struct copied-syntax added-syntax (source-path)
  #:transparent
  #:sealed
  #:guard (struct-guard/c syntax-path?))


(struct splice-replacement (start-path replaced-children-count new-children)
  #:transparent
  #:guard (struct-guard/c proper-syntax-path? exact-nonnegative-integer? (treelist/c added-syntax?))
  #:sealed)


(struct syntax-delta (replacements)
  #:transparent
  #:constructor-name constructor:syntax-delta
  #:omit-define-syntaxes
  #:sealed)


(define (syntax-delta replacements)
  (define sorted
    (transduce replacements
               (indexing splice-replacement-start-path)
               #:into (into-sorted-map syntax-path<=>)))
  (for/fold ([previous #false] #:result (void))
            ([replacement (in-sorted-map-values sorted)])
    (when previous
      (check-splice-replacements-disjoint previous replacement))
    replacement)
  (constructor:syntax-delta sorted))


(define (check-splice-replacements-disjoint replacement next-replacement)
  (define start (splice-replacement-start-path replacement))
  (define children-count (splice-replacement-replaced-children-count replacement))
  (define next-start (splice-replacement-start-path next-replacement))
  (define disjoint?
    (cond
      [(equal? children-count 0) #true]
      [(empty-syntax-path? start) #false]
      [(empty-syntax-path? next-start) #false]
      [(equal? (syntax-path-parent start) (syntax-path-parent next-start))
       (<= (+ (syntax-path-last-element start)) (syntax-path-last-element next-start))]
      [else #true]))
  (unless disjoint?
    (raise-arguments-error 'syntax-delta "overlapping splices"
                           "replacement" replacement
                           "overlapping replacement" next-replacement)))


(define (syntax-apply-delta orig-stx delta)
  (define replacements (syntax-delta-replacements delta))
  (for/fold ([stx orig-stx])
            ([replacement (in-sorted-map-values replacements #:descending? #true)])
    (match-define (splice-replacement start children-count new-children) replacement)
    (define new-stxs
      (for/list ([added (in-treelist new-children)])
        (match added
          [(new-syntax new-stx) new-stx]
          [(copied-syntax orig-path) (syntax-ref stx orig-path)])))
    (syntax-insert-splice (syntax-remove-splice stx start children-count) start new-stxs)))


; TODO: more test cases
(module+ test
  (test-case "syntax-apply-delta basic functionality"
    (define stx
      #'(module foo racket
          (begin
            (define x 1)
            (define y 2))))
    (define begin-path (syntax-path (list 3)))
    (define copied-x-def (copied-syntax (syntax-path (list 3 1))))
    (define copied-y-def (copied-syntax (syntax-path (list 3 2))))
    (define splice (splice-replacement begin-path 1 (treelist copied-x-def copied-y-def)))
    (define delta (syntax-delta (list splice)))

    (define actual (syntax-apply-delta stx delta))

    (define expected
      #'(module foo racket
          (define x 1)
          (define y 2)))
    ; Test should now pass since syntax-insert-splice and syntax-remove-splice are implemented
    (check-equal? (syntax->datum actual) (syntax->datum expected)))

  (test-case "multiple splice replacements"
    (define stx #'(a b c d e f))
    (define splice1 (splice-replacement (syntax-path (list 1)) 2 (treelist (new-syntax #'x))))
    (define splice2 (splice-replacement (syntax-path (list 4)) 1 (treelist (new-syntax #'y) (new-syntax #'z))))
    (define delta (syntax-delta (list splice1 splice2)))

    (define actual (syntax-apply-delta stx delta))
    (define expected #'(a x d y z f))
    (check-equal? (syntax->datum actual) (syntax->datum expected)))

  (test-case "empty splice replacement"
    (define stx #'(a b c))
    (define splice (splice-replacement (syntax-path (list 1)) 1 (treelist)))
    (define delta (syntax-delta (list splice)))

    (define actual (syntax-apply-delta stx delta))
    (define expected #'(a c))
    (check-equal? (syntax->datum actual) (syntax->datum expected)))

  (test-case "copying from different parts of syntax"
    (define stx #'((foo 1) (bar 2) (baz 3)))
    (define copied-foo (copied-syntax (syntax-path (list 0 0))))
    (define copied-bar-val (copied-syntax (syntax-path (list 1 1))))
    (define splice (splice-replacement (syntax-path (list 1)) 1 (treelist copied-foo copied-bar-val)))
    (define delta (syntax-delta (list splice)))

    (define actual (syntax-apply-delta stx delta))
    (define expected #'((foo 1) foo 2 (baz 3)))
    (check-equal? (syntax->datum actual) (syntax->datum expected))))
