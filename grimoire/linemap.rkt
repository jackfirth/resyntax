#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [string-linemap (-> string? linemap?)]
  [linemap? (-> any/c boolean?)]
  [linemap-position-to-line (-> linemap? exact-nonnegative-integer? exact-positive-integer?)]
  [linemap-position-to-start-of-line
   (-> linemap? exact-nonnegative-integer? exact-nonnegative-integer?)]
  [linemap-position-to-end-of-line
   (-> linemap? exact-nonnegative-integer? exact-nonnegative-integer?)]
  [syntax-line-range (-> syntax? #:linemap linemap? range?)]))


(require rebellion/base/comparator
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/entry
         rebellion/collection/sorted-map
         rebellion/collection/vector/builder)


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
       (define last-line (string->immutable-string (substring str line-start-index index)))
       (vector-builder-add lines last-line)
       (sorted-map-builder-put line-numbers-by-start-position line-start-index line-number)
       (vector-builder-add start-positions-by-line-number line-start-index)]
      [(equal? (string-ref str index) #\newline)
       (define next-line (string->immutable-string (substring str line-start-index index)))
       (vector-builder-add lines next-line)
       (sorted-map-builder-put line-numbers-by-start-position line-start-index line-number)
       (vector-builder-add start-positions-by-line-number line-start-index)
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


(define (syntax-line-range stx #:linemap map)
  (define first-line (syntax-line stx))
  ;; Syntax object positions are one-indexed, unlike linemap positions.
  (define last-line
    (linemap-position-to-line map (+ (sub1 (syntax-position stx)) (syntax-span stx))))
  (unless (<= first-line last-line)
    (raise-arguments-error 'syntax-line-range
                           "syntax object's last line number is before its first line number"
                           "syntax" stx
                           "first line" first-line
                           "last line" last-line
                           "position" (syntax-position stx)
                           "span" (syntax-span stx)))
  (closed-range first-line last-line #:comparator natural<=>))


(module+ test

  (test-case (name-string string-linemap)

    (define (nat-map . args)
      (apply sorted-map #:key-comparator natural<=> args))
    
    (check-equal? (string-linemap "") (linemap #("") (nat-map 0 1) #(0)))
    (check-equal? (string-linemap "a") (linemap #("a") (nat-map 0 1) #(0)))
    (check-equal? (string-linemap "λ") (linemap #("λ") (nat-map 0 1) #(0)))
    (check-equal? (string-linemap "a\n") (linemap #("a" "") (nat-map 0 1 2 2) #(0 2)))
    (check-equal? (string-linemap "λ\n") (linemap #("λ" "") (nat-map 0 1 2 2) #(0 2)))
    (check-equal? (string-linemap "aaa\n") (linemap #("aaa" "") (nat-map 0 1 4 2) #(0 4)))
    (check-equal? (string-linemap "aaa\nbbb") (linemap #("aaa" "bbb") (nat-map 0 1 4 2) #(0 4)))
    (check-equal? (string-linemap "aaa\nbbb\n")
                  (linemap #("aaa" "bbb" "") (nat-map 0 1 4 2 8 3) #(0 4 8)))
    (check-equal? (string-linemap "a\n\n\nb")
                  (linemap #("a" "" "" "b") (nat-map 0 1 2 2 3 3 4 4) #(0 2 3 4))))
  
  (test-case (name-string linemap-position-to-line)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-position-to-line map 0) 1)
      (check-equal? (linemap-position-to-line map 1) 1)
      (check-equal? (linemap-position-to-line map 2) 1)
      (check-equal? (linemap-position-to-line map 3) 1)
      (check-equal? (linemap-position-to-line map 4) 1)
      (check-equal? (linemap-position-to-line map 5) 1)
      (check-equal? (linemap-position-to-line map 6) 2)
      (check-equal? (linemap-position-to-line map 7) 2)
      (check-equal? (linemap-position-to-line map 8) 2)
      (check-equal? (linemap-position-to-line map 9) 2)
      (check-equal? (linemap-position-to-line map 10) 2)
      (check-equal? (linemap-position-to-line map 11) 2)
      (check-equal? (linemap-position-to-line map 12) 3))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-position-to-line map 0) 1)
      (check-equal? (linemap-position-to-line map 1) 1)
      (check-equal? (linemap-position-to-line map 2) 2)
      (check-equal? (linemap-position-to-line map 3) 3)
      (check-equal? (linemap-position-to-line map 4) 3)))

  (test-case (name-string linemap-line-start-position)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-line-start-position map 1) 0)
      (check-equal? (linemap-line-start-position map 2) 6)
      (check-equal? (linemap-line-start-position map 3) 12))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-line-start-position map 1) 0)
      (check-equal? (linemap-line-start-position map 2) 2)
      (check-equal? (linemap-line-start-position map 3) 3)))

  (test-case (name-string linemap-line-end-position)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-line-end-position map 1) 5)
      (check-equal? (linemap-line-end-position map 2) 11))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-line-end-position map 1) 1)
      (check-equal? (linemap-line-end-position map 2) 2)
      (check-equal? (linemap-line-end-position map 3) 4)))

  (test-case (name-string linemap-position-to-start-of-line)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-position-to-start-of-line map 0) 0)
      (check-equal? (linemap-position-to-start-of-line map 1) 0)
      (check-equal? (linemap-position-to-start-of-line map 2) 0)
      (check-equal? (linemap-position-to-start-of-line map 3) 0)
      (check-equal? (linemap-position-to-start-of-line map 4) 0)
      (check-equal? (linemap-position-to-start-of-line map 5) 0)
      (check-equal? (linemap-position-to-start-of-line map 6) 6)
      (check-equal? (linemap-position-to-start-of-line map 7) 6)
      (check-equal? (linemap-position-to-start-of-line map 8) 6)
      (check-equal? (linemap-position-to-start-of-line map 9) 6)
      (check-equal? (linemap-position-to-start-of-line map 10) 6)
      (check-equal? (linemap-position-to-start-of-line map 11) 6)
      (check-equal? (linemap-position-to-start-of-line map 12) 12))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-position-to-start-of-line map 0) 0)
      (check-equal? (linemap-position-to-start-of-line map 1) 0)
      (check-equal? (linemap-position-to-start-of-line map 2) 2)
      (check-equal? (linemap-position-to-start-of-line map 3) 3)
      (check-equal? (linemap-position-to-start-of-line map 4) 3)))

  (test-case (name-string linemap-position-to-end-of-line)

    (test-case "two-line string with ending newline"
      (define map (string-linemap "hello\nworld\n"))
      (check-equal? (linemap-position-to-end-of-line map 0) 5)
      (check-equal? (linemap-position-to-end-of-line map 1) 5)
      (check-equal? (linemap-position-to-end-of-line map 2) 5)
      (check-equal? (linemap-position-to-end-of-line map 3) 5)
      (check-equal? (linemap-position-to-end-of-line map 4) 5)
      (check-equal? (linemap-position-to-end-of-line map 5) 5)
      (check-equal? (linemap-position-to-end-of-line map 6) 11)
      (check-equal? (linemap-position-to-end-of-line map 7) 11)
      (check-equal? (linemap-position-to-end-of-line map 8) 11)
      (check-equal? (linemap-position-to-end-of-line map 9) 11)
      (check-equal? (linemap-position-to-end-of-line map 10) 11)
      (check-equal? (linemap-position-to-end-of-line map 11) 11)
      (check-equal? (linemap-position-to-end-of-line map 12) 12))

    (test-case "multiple blank lines"
      (define map (string-linemap "a\n\nb"))
      (check-equal? (linemap-position-to-end-of-line map 0) 1)
      (check-equal? (linemap-position-to-end-of-line map 1) 1)
      (check-equal? (linemap-position-to-end-of-line map 2) 2)
      (check-equal? (linemap-position-to-end-of-line map 3) 4)
      (check-equal? (linemap-position-to-end-of-line map 4) 4))))
