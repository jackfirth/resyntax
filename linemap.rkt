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
  [linemap-position-to-end-of-line (-> linemap? exact-positive-integer? exact-positive-integer?)]
  [syntax-start-line-position (-> syntax? #:linemap linemap? exact-positive-integer?)]
  [syntax-end-line-position (-> syntax? #:linemap linemap? exact-positive-integer?)]))


(require rebellion/base/comparator
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/sorted-map
         rebellion/collection/vector/builder
         rebellion/private/guarded-block)


(module+ test
  (require rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct linemap (lines line-numbers-by-start-position start-positions-by-line-number) #:transparent)


(define (string-linemap str)
  (define lines (make-vector-builder))
  (define line-numbers-by-start-position (make-sorted-map-builder natural<=>))
  (define start-positions-by-line-number (make-vector-builder))
  (define char-count (string-length str))
  (let loop ([line-number 1] [line-start-index 0] [index 0])
    (cond
      [(= index char-count)
       (define last-line (substring str line-start-index index))
       (vector-builder-add lines last-line)
       (sorted-map-builder-put line-numbers-by-start-position (add1 line-start-index) line-number)
       (vector-builder-add start-positions-by-line-number (add1 line-start-index))]
      [(equal? (string-ref str index) #\newline)
       (define next-line (substring str line-start-index index))
       (vector-builder-add lines next-line)
       (sorted-map-builder-put line-numbers-by-start-position (add1 line-start-index) line-number)
       (vector-builder-add start-positions-by-line-number (add1 line-start-index))
       (define next-index (add1 index))
       (loop (add1 line-number) next-index next-index)]
      [else (loop line-number line-start-index (add1 index))]))
  (linemap (build-vector lines)
           (build-sorted-map line-numbers-by-start-position)
           (build-vector start-positions-by-line-number)))


(define (linemap-line map line)
  (vector-ref (linemap-lines map) (sub1 line)))


(define (linemap-position-to-line map position)
  (define line-numbers (linemap-line-numbers-by-start-position map))
  (entry-value (present-value (sorted-map-entry-at-most line-numbers position))))


(define (linemap-line-start-position map line)
  (vector-ref (linemap-start-positions-by-line-number map) (sub1 line)))


(define (linemap-line-end-position map line)
  (+ (linemap-line-start-position map line)
     (string-length (linemap-line map line))))


(define (linemap-position-to-start-of-line map position)
  (linemap-line-start-position map (linemap-position-to-line map position)))


(define (linemap-position-to-end-of-line map position)
  (linemap-line-end-position map (linemap-position-to-line map position)))


(define (syntax-start-line-position stx #:linemap map)
  (linemap-position-to-start-of-line map (syntax-position stx)))


(define (syntax-end-line-position stx #:linemap map)
  (linemap-position-to-end-of-line map (+ (syntax-position stx) (syntax-span stx))))


(module+ test

  (test-case (name-string string-linemap)

    (define (nat-map . args)
      (apply sorted-map #:key-comparator natural<=> args))
    
    (check-equal? (string-linemap "") (linemap #("") (nat-map 1 1) #(1)))
    (check-equal? (string-linemap "a") (linemap #("a") (nat-map 1 1) #(1)))
    (check-equal? (string-linemap "λ") (linemap #("λ") (nat-map 1 1) #(1)))
    (check-equal? (string-linemap "a\n") (linemap #("a" "") (nat-map 1 1 3 2) #(1 3)))
    (check-equal? (string-linemap "λ\n") (linemap #("λ" "") (nat-map 1 1 3 2) #(1 3)))
    (check-equal? (string-linemap "aaa\n") (linemap #("aaa" "") (nat-map 1 1 5 2) #(1 5)))
    (check-equal? (string-linemap "aaa\nbbb") (linemap #("aaa" "bbb") (nat-map 1 1 5 2) #(1 5)))
    (check-equal? (string-linemap "aaa\nbbb\n")
                  (linemap #("aaa" "bbb" "") (nat-map 1 1 5 2 9 3) #(1 5 9)))
    (check-equal? (string-linemap "a\n\n\nb")
                  (linemap #("a" "" "" "b") (nat-map 1 1 3 2 4 3 5 4) #(1 3 4 5))))
  
  (test-case (name-string linemap-position-to-line)

    (test-case "two-line string with ending newline"
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
      (check-equal? (linemap-position-to-line map 13) 3))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-position-to-line map 1) 1)
      (check-equal? (linemap-position-to-line map 2) 1)
      (check-equal? (linemap-position-to-line map 3) 2)
      (check-equal? (linemap-position-to-line map 4) 3)
      (check-equal? (linemap-position-to-line map 5) 3)))

  (test-case (name-string linemap-line-start-position)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-line-start-position map 1) 1)
      (check-equal? (linemap-line-start-position map 2) 7)
      (check-equal? (linemap-line-start-position map 3) 13))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-line-start-position map 1) 1)
      (check-equal? (linemap-line-start-position map 2) 3)
      (check-equal? (linemap-line-start-position map 3) 4)))

  (test-case (name-string linemap-line-end-position)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-line-end-position map 1) 6)
      (check-equal? (linemap-line-end-position map 2) 12))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-line-end-position map 1) 2)
      (check-equal? (linemap-line-end-position map 2) 3)
      (check-equal? (linemap-line-end-position map 3) 5)))

  (test-case (name-string linemap-position-to-start-of-line)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-position-to-start-of-line map 1) 1)
      (check-equal? (linemap-position-to-start-of-line map 2) 1)
      (check-equal? (linemap-position-to-start-of-line map 3) 1)
      (check-equal? (linemap-position-to-start-of-line map 4) 1)
      (check-equal? (linemap-position-to-start-of-line map 5) 1)
      (check-equal? (linemap-position-to-start-of-line map 6) 1)
      (check-equal? (linemap-position-to-start-of-line map 7) 7)
      (check-equal? (linemap-position-to-start-of-line map 8) 7)
      (check-equal? (linemap-position-to-start-of-line map 9) 7)
      (check-equal? (linemap-position-to-start-of-line map 10) 7)
      (check-equal? (linemap-position-to-start-of-line map 11) 7)
      (check-equal? (linemap-position-to-start-of-line map 12) 7)
      (check-equal? (linemap-position-to-start-of-line map 13) 13))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-position-to-start-of-line map 1) 1)
      (check-equal? (linemap-position-to-start-of-line map 2) 1)
      (check-equal? (linemap-position-to-start-of-line map 3) 3)
      (check-equal? (linemap-position-to-start-of-line map 4) 4)
      (check-equal? (linemap-position-to-start-of-line map 5) 4)))

  (test-case (name-string linemap-position-to-end-of-line)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-position-to-end-of-line map 1) 6)
      (check-equal? (linemap-position-to-end-of-line map 2) 6)
      (check-equal? (linemap-position-to-end-of-line map 3) 6)
      (check-equal? (linemap-position-to-end-of-line map 4) 6)
      (check-equal? (linemap-position-to-end-of-line map 5) 6)
      (check-equal? (linemap-position-to-end-of-line map 6) 6)
      (check-equal? (linemap-position-to-end-of-line map 7) 12)
      (check-equal? (linemap-position-to-end-of-line map 8) 12)
      (check-equal? (linemap-position-to-end-of-line map 9) 12)
      (check-equal? (linemap-position-to-end-of-line map 10) 12)
      (check-equal? (linemap-position-to-end-of-line map 11) 12)
      (check-equal? (linemap-position-to-end-of-line map 12) 12))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-position-to-end-of-line map 1) 2)
      (check-equal? (linemap-position-to-end-of-line map 2) 2)
      (check-equal? (linemap-position-to-end-of-line map 3) 3)
      (check-equal? (linemap-position-to-end-of-line map 4) 5)
      (check-equal? (linemap-position-to-end-of-line map 5) 5))))
