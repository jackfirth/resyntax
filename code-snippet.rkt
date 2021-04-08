#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [code-snippet? predicate/c]
  [code-snippet
   (-> string? exact-nonnegative-integer? exact-positive-integer? code-snippet?)]
  [code-snippet-raw-text (-> code-snippet? immutable-string?)]
  [code-snippet-start-column (-> code-snippet? exact-nonnegative-integer?)]
  [code-snippet-start-line (-> code-snippet? exact-positive-integer?)]
  [code-snippet-end-line (-> code-snippet? exact-positive-integer?)]))


(require racket/format
         racket/math
         racket/sequence
         racket/string
         rebellion/base/immutable-string)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(struct code-snippet (raw-text start-column start-line)
  #:transparent

  #:guard
  (λ (raw-text start-column start-line _)
    (values (string->immutable-string raw-text) start-column start-line))
  
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (define start-line (code-snippet-start-line this))
     (define end-line (code-snippet-end-line this))
     (define end-line-digit-count (digit-count end-line))
     (define leading-indentation (code-snippet-start-column this))
     (for ([line (in-lines (open-input-string (code-snippet-raw-text this)))]
           [n (in-naturals)])
       (define line-number-string
         (~a (+ n start-line) #:min-width end-line-digit-count))
       (write-string line-number-string out)
       (write-string " " out)
       (cond
         [(zero? n) (write-string line out)]
         [else
          (write-string (if (string-prefix? line leading-indentation)
                            (substring line leading-indentation)
                            line)
                        out)])
       (newline out)))])

; code-snippet-start-line and code-snippet-end-line give an inclusive-exclusive range; that is,
; a one-line snippet on line 10 would have start and end lines of 10 and 11, respectively.
(define (code-snippet-end-line snippet)
  (define line-count (sequence-length (in-lines (open-input-string (code-snippet-raw-text snippet)))))
  (+ (code-snippet-start-line snippet) line-count))


(module+ test
  (test-case (name-string code-snippet-end-line)
    (test-case "end-line is one greater than the number of the last line in the snippet"
      (define snippet (code-snippet "(+\n 1\n 2\n 3)" 0 6))
      (check-equal? (code-snippet-end-line snippet) 10)))

  (test-case (name-string code-snippet)

    (test-case "one-line snippet"
      (define snippet (code-snippet "(+ 1 2 3)" 0 1))
      (check-equal? (~a snippet) "1 (+ 1 2 3)\n"))

    (test-case "multiline snippet"
      (define snippet (code-snippet "(define (f x)\n  (+ x x))" 0 1))
      (check-equal? (~a snippet) "1 (define (f x)\n2   (+ x x))\n"))

    (test-case "indented snippet"
      (define snippet (code-snippet "(+ 1 2 3)" 10 1))
      (check-equal? (~a snippet) "1 (+ 1 2 3)\n"))

    (test-case "indented multiline snippet"
      (define snippet (code-snippet "(define (f x)\n            (+ x x))" 10 1))
      (check-equal? (~a snippet) "1 (define (f x)\n2   (+ x x))\n"))

    (test-case "snippet with line numbers of different digit counts"
      (define snippet (code-snippet "(+ 1 2 3)\n(+ 4 5 6)" 0 99))
      (check-equal? (~a snippet) "99  (+ 1 2 3)\n100 (+ 4 5 6)\n"))))


(define (digit-count n)
  (add1 (exact-floor (log n 10))))


(module+ test
  (test-case (name-string digit-count)
    (check-equal? (digit-count 1) 1)
    (check-equal? (digit-count 123) 3)
    (check-equal? (digit-count 10000) 5)
    (check-equal? (digit-count 10001) 5)
    (check-equal? (digit-count 9999) 4)))
