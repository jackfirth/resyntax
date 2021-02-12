#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [graph? predicate/c]
  [graph (->* () (#:vertex-count (or/c natural? #false)) #:rest (listof entry?) graph?)]
  [graph-reverse (-> graph? graph?)]
  [graph-vertex-count (-> graph? natural?)]
  [graph-cycle-vertices (-> graph? (listof natural?))]
  [graph-successors (-> graph? natural? (listof natural?))]
  [graph-predecessors (-> graph? natural? (listof natural?))]
  [edges->graph (->* ((sequence/c entry?)) (#:vertex-count (or/c natural? #false)) graph?)]
  [in-graph-edges (-> graph? (sequence/c entry?))]))


(require racket/list
         racket/match
         racket/math
         racket/sequence
         racket/stream
         rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/transducer
         rebellion/streaming/reducer)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(struct graph (adjacency-vectors reverse-adjacency-vectors)
  #:transparent
  #:constructor-name make-graph
  #:omit-define-syntaxes)


(define (graph #:vertex-count [vertex-count #false] . edge-list)
  (edges->graph edge-list #:vertex-count vertex-count))


(define edge<=>
  (comparator-chain
                 (comparator-map natural<=> entry-key)
                 (comparator-map natural<=> entry-value)))

(define (edges->graph edges #:vertex-count [vertex-count #false])
  (define edge-list (sequence->list edges))
  (define max-vertex
    (transduce edge-list
               (append-mapping (λ (e) (list (entry-key e) (entry-value e))))
               #:into (into-max)))
  (define vertex-count* (or vertex-count (option-case max-vertex #:present add1 #:absent (λ () 0))))
  (define sorted-edges
    (transduce edge-list
               (sorting edge<=>)
               #:into into-list))
  (define sorted-reverse-edges
    (transduce edge-list
               (mapping (λ (e) (entry (entry-value e) (entry-key e))))
               (sorting edge<=>)
               #:into into-list))
  (define (build-adjacency-vectors edges)
    (define dep-vectors (make-vector vertex-count* #()))
    (define (add! i depstack)
      (vector-set! dep-vectors i (vector->immutable-vector (list->vector (reverse depstack)))))
    (for/fold ([i 0]
               [current-depstack '()]
               #:result
               (unless (empty? current-depstack)
                 (add! i current-depstack)))
              ([edge (in-list edges)])
      (match-define (entry dependant dependency) edge)
      (cond
        [(equal? i dependant) (values i (cons dependency current-depstack))]
        [else
         (add! i current-depstack)
         (values dependant (list dependency))]))
    (vector->immutable-vector dep-vectors))
  (make-graph (build-adjacency-vectors sorted-edges) (build-adjacency-vectors sorted-reverse-edges)))


(define (in-graph-edges g)
  (define adjacency-vectors (graph-adjacency-vectors g))
  (define vertex-count (vector-length adjacency-vectors))
  (for*/stream ([i (in-range 0 vertex-count)]
                [j (in-vector (vector-ref adjacency-vectors i))])
    (entry i j)))


(define (graph-successors g i)
  (sequence->list (vector-ref (graph-adjacency-vectors g) i)))


(define (graph-predecessors g i)
  (sequence->list (vector-ref (graph-reverse-adjacency-vectors g) i)))


(define (graph-reverse g)
  (make-graph (graph-reverse-adjacency-vectors g) (graph-adjacency-vectors g)))


(define (graph-vertex-count g)
  (vector-length (graph-adjacency-vectors g)))


(define (cycle-starting-at g i)
  (define vectors (graph-adjacency-vectors g))
  (define/guard (loop [i i] [visited '()])
    (guard-match (? integer? previous) (index-of visited i) then
      (reverse (take visited (add1 previous))))
    (for/or ([child (in-vector (vector-ref vectors i))])
      (loop child (cons i visited))))
  (or (loop) '()))


(define (graph-cycle-vertices g)
  (define cycle-vertices (make-vector (graph-vertex-count g) #false))
  (for ([i (in-range (graph-vertex-count g))]
        #:unless (vector-ref cycle-vertices i))
    (for ([v (in-list (cycle-starting-at g i))])
      (vector-set! cycle-vertices v #true)))
  (for/list ([i (in-range (graph-vertex-count g))]
             #:when (vector-ref cycle-vertices i))
    i))


(module+ test
  (test-case (name-string graph-cycle-vertices)
    (test-case "empty graph has no cycles"
      (define g (graph))
      (check-equal? (graph-vertex-count g) 0)
      (check-equal? (graph-cycle-vertices g) '()))

    (test-case "graph with no edges has no cycles"
      (define g (graph #:vertex-count 5))
      (check-equal? (graph-vertex-count g) 5)
      (check-equal? (graph-cycle-vertices g) '()))

    (test-case "self-connected vertices are cycles"
      (define g (graph (entry 2 2) (entry 3 3)))
      (check-equal? (graph-cycle-vertices g) (list 2 3)))

    (test-case "mutually-connected vertices are cycles"
      (define g (graph (entry 2 3) (entry 3 4) (entry 4 2)))
      (check-equal? (graph-cycle-vertices g) (list 2 3 4)))

    (test-case "single edges aren't cycles"
      (define g (graph (entry 2 3)))
      (check-equal? (graph-cycle-vertices g) '()))

    (test-case "edges into cycles don't expand cycles"
      (define g (graph (entry 0 2) (entry 2 3) (entry 3 2)))
      (check-equal? (graph-cycle-vertices g) (list 2 3))))

  (test-case (name-string in-graph-edges)
    (define g (graph (entry 0 2) (entry 1 3) (entry 1 0) (entry 7 0)))
    (define actual (sequence->list (in-graph-edges g)))
    (check-equal? actual (list (entry 0 2) (entry 1 0) (entry 1 3) (entry 7 0))))

  (test-case (name-string graph-predecessors)
    (define g (graph (entry 1 2) (entry 2 2)))
    (check-equal? (graph-predecessors g 2) (list 1 2))))
