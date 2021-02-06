#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [code-snippet? predicate/c]
  [code-snippet
   (-> immutable-string? exact-nonnegative-integer? exact-positive-integer? code-snippet?)]
  [code-snippet-raw-text (-> code-snippet? immutable-string?)]
  [code-snippet-start-column (-> code-snippet? exact-nonnegative-integer?)]
  [code-snippet-start-line (-> code-snippet? exact-positive-integer?)]))


(require racket/format
         racket/math
         racket/sequence
         rebellion/base/immutable-string)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct code-snippet (raw-text start-column start-line)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (define end-line (code-snippet-end-line this))
     (for ([line (in-lines (open-input-string (code-snippet-raw-text this)))]
           [n (in-naturals)])
       (define line-number-string
         (~a (+ n (code-snippet-start-line this)) #:min-width (digit-count end-line)))
       (write-string line-number-string out)
       (write-string " " out)
       (when (zero? n)
         (write-string (make-string (code-snippet-start-column this) #\space) out))
       (write-string line out)
       (newline out)))])


(define (code-snippet-end-line snippet)
  (define line-count (sequence-length (in-lines (open-input-string (code-snippet-raw-text snippet)))))
  (+ (code-snippet-start-line snippet) line-count))


(module+ test
  (test-case (name-string code-snippet)

    (test-case "one-line snippet"
      (define snippet (code-snippet "(+ 1 2 3)" 0 1))
      (check-equal? (~a snippet) "1 (+ 1 2 3)"))

    (test-case "indented snippet"
      (define snippet (code-snippet "(+ 1 2 3)" 10 1))
      (check-equal? (~a snippet) "1           (+ 1 2 3)"))

    (test-case "multiline snippet"
      (define snippet (code-snippet "(define (f x)\n  (+ x x))" 0 1))
      (check-equal? (~a snippet) "1 (define (f x)\n2   (+ x x))"))

    (test-case "snippet with line numbers of different digit counts"
      (define snippet (code-snippet "(+ 1 2 3)\n(+ 4 5 6)" 0 99))
      (check-equal? (~a snippet) "99  (+ 1 2 3)\n100 (+ 4 5 6)"))))


(define (digit-count n)
  (add1 (exact-floor (log n 10))))


(module+ test
  (test-case (name-string digit-count)
    (check-equal? (digit-count 1) 1)
    (check-equal? (digit-count 123) 3)
    (check-equal? (digit-count 10000) 5)
    (check-equal? (digit-count 10001) 5)
    (check-equal? (digit-count 9999) 4)))
