#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [string-linemap (-> string? linemap?)]
  [linemap? predicate/c]
  [linemap-lines (-> linemap? (vectorof (and/c string? immutable?) #:immutable #true #:flat? #true))]
  [linemap-position-to-line (-> linemap? exact-positive-integer? exact-positive-integer?)]
  [linemap-line-start-position (-> linemap? exact-positive-integer? exact-positive-integer?)]
  [linemap-position-to-start-of-line (-> linemap? exact-positive-integer? exact-positive-integer?)]
  [linemap-position-to-end-of-line (-> linemap? exact-positive-integer? exact-positive-integer?)]))


(require rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/sorted-map
         rebellion/collection/vector/builder)


(module+ test
  (require rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct linemap (lines line-numbers-by-end-position start-positions-by-line-number) #:transparent)


(define (string-linemap str)
  (define lines (make-vector-builder))
  (define line-numbers-by-end-position (make-sorted-map-builder natural<=>))
  (define start-positions-by-line-number (make-vector-builder))
  (sorted-map-builder-put line-numbers-by-end-position 1 1)
  (vector-builder-add start-positions-by-line-number 1)
  (define in (open-input-string str))
  (for ([line (in-lines in)]
        [line-number (in-naturals 2)])
    (vector-builder-add lines (string->immutable-string line))
    (define-values (unused-line unused-col position) (port-next-location in))
    (sorted-map-builder-put line-numbers-by-end-position position line-number)
    (vector-builder-add start-positions-by-line-number position))
  (linemap (build-vector lines)
           (build-sorted-map line-numbers-by-end-position)
           (build-vector start-positions-by-line-number)))


(define (linemap-position-to-line map position)
  (entry-value
   (present-value (sorted-map-entry-at-most (linemap-line-numbers-by-end-position map) position))))


(define (linemap-line-start-position map line)
  (vector-ref (linemap-start-positions-by-line-number map) (sub1 line)))


(define (linemap-line-end-position map line)
  (+ (linemap-line-start-position map line)
     (string-length (vector-ref (linemap-lines map) (sub1 line)))))


(define (linemap-position-to-start-of-line map position)
  (linemap-line-start-position (linemap-position-to-line map position)))


(define (linemap-position-to-end-of-line map position)
  (linemap-line-end-position (linemap-position-to-line map position)))


(module+ test
  (test-case (name-string string-linemap)
    (define map (string-linemap "hello\nworld\n"))
    (check-equal? (linemap-position-to-line map 1) 1)
    (check-equal? (linemap-position-to-line map 2) 1)
    (check-equal? (linemap-position-to-line map 3) 1)
    (check-equal? (linemap-position-to-line map 4) 1)
    (check-equal? (linemap-position-to-line map 5) 1)
    (check-equal? (linemap-position-to-line map 6) 1)
    (check-equal? (linemap-position-to-line map 7) 2)
    (check-equal? (linemap-position-to-line map 8) 2)
    (check-equal? (linemap-position-to-line map 9) 2)
    (check-equal? (linemap-position-to-line map 10) 2)
    (check-equal? (linemap-position-to-line map 11) 2)
    (check-equal? (linemap-position-to-line map 12) 2)
    (check-equal? (linemap-position-to-line map 13) 3)
    (check-equal? (linemap-line-start-position map 1) 1)
    (check-equal? (linemap-line-start-position map 2) 7)
    (check-equal? (linemap-line-start-position map 3) 13)))