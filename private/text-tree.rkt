#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [text-tree? (-> any/c boolean?)]
  [text-tree
   (->* ((sequence/c (or/c string? text-tree?)))
        (#:separators (or/c #false string? (sequence/c string?))
         #:hanging-line-prefixes (or/c #false string? (sequence/c string?)))
        text-tree?)]
  [text-tree-children
   (-> text-tree? (vectorof (or/c immutable-string? text-tree?) #:immutable #true))]
  [text-tree-separators (-> text-tree? (vectorof immutable-string? #:immutable #true))]
  [text-tree-hanging-line-prefixes (-> text-tree? (vectorof immutable-string? #:immutable #true))]
  [text-tree-render (-> text-tree? immutable-string?)]))


(require guard
         racket/generator
         racket/mutability
         racket/sequence
         racket/string)


(module+ test
  (require (submod "..")
           rackunit))


(define (zip-between longer-vs shorter-vs)
  (in-generator
   (for ([v1 longer-vs]
         [v2 (sequence-append (list #false) shorter-vs)]
         [i (in-naturals)])
     (unless (zero? i)
       (yield v2))
     (yield v1))))


(module+ test
  (test-case "zip-between"
    (check-equal? (sequence->list (zip-between '(a b c d) '(1 2 3))) '(a 1 b 2 c 3 d))))


(struct text-tree (children separators hanging-line-prefixes)

  #:transparent
  #:constructor-name constructor:text-tree
  #:omit-define-syntaxes

  #:guard
  (λ (children separators line-prefixes type-name)
    (set!-values (children separators line-prefixes)
                 (text-tree-contract-guard children separators line-prefixes type-name))
    (set! children (normalize-text-tree-children children))
    (define child-count (vector-length children))
    (define required-separator-count (add1 child-count))
    (set! separators (normalize-string-vector separators required-separator-count))

    (unless (equal? (vector-length separators) required-separator-count)
      (raise-arguments-error
       'text-tree
       "wrong number of separators provided, expected one more than the number of children"
       "separator count" (vector-length separators)
       "child count" child-count
       "separators" separators
       "children" children))
    
    (set! line-prefixes (normalize-string-vector line-prefixes child-count))

    (unless (equal? (vector-length line-prefixes) child-count)
      (raise-arguments-error
       'text-tree
       "wrong number of hanging line prefixes provided, expected one for each child"
       "line prefix count" (vector-length line-prefixes)
       "child count" child-count
       "line prefixes" line-prefixes
       "children" children))

    (values children separators line-prefixes)))


(define text-tree-contract-guard
  (struct-guard/c (sequence/c (or/c string? text-tree?))
                  (or/c #false string? (sequence/c string?))
                  (or/c #false string? (sequence/c string?))))


(define (normalize-text-tree-children children)
  (vector->immutable-vector
   (for/vector ([child children])
     (if (string? child)
         (string->immutable-string child)
         child))))


(define (text-tree children
                   #:separators [separators #false]
                   #:hanging-line-prefixes [line-prefixes #false])
  (constructor:text-tree children separators line-prefixes))


(define (normalize-string-vector strings required-count)
  (cond
    [(not strings) (vector->immutable-vector (make-vector required-count ""))]
    [(string? strings) (vector->immutable-vector (make-vector required-count strings))]
    [else (vector->immutable-vector (for/vector ([s strings]) (string->immutable-string s)))]))


(define/guard (text-tree-render tree)
  (guard (text-tree? tree) #:else (string->immutable-string tree))
  (define child-strings
    (for/list ([child (in-vector (text-tree-children tree))]
               [line-prefix (in-vector (text-tree-hanging-line-prefixes tree))])
      (string-add-hanging-line-prefix (text-tree-render child) line-prefix)))
  (string->immutable-string
   (string-join
    (sequence->list
     (zip-between (text-tree-separators tree) child-strings))
    "")))


(define (string-add-hanging-line-prefix s prefix)
  (string-join (string-split s "\n") (string-append "\n" prefix)))
