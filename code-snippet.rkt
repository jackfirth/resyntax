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


(require math/number-theory
         racket/format
         racket/sequence
         rebellion/base/immutable-string)


;@----------------------------------------------------------------------------------------------------


(struct code-snippet (raw-text start-column start-line)
  #:transparent
  #:methods gen:custom-write
  [(define (write-proc this out mode)
     (define end-line (code-snippet-end-line this))
     (for ([line (in-lines (open-input-string (code-snippet-raw-text this)))]
           [n (in-naturals)])
       (write-string
        (~r (+ n (code-snippet-start-line this))
            #:min-width (digit-count end-line))
        out)
       (write-string "  " out)
       (when (zero? n)
         (write-string (make-string (code-snippet-start-column this) #\space) out))
       (write-string line out)
       (newline out)))])


(define (code-snippet-end-line snippet)
  (define line-count (sequence-length (in-lines (open-input-string (code-snippet-raw-text snippet)))))
  (+ (code-snippet-start-line snippet) line-count))


(define (digit-count n)
  (add1 (max-dividing-power 10 n)))
