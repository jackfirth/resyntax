#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [oneline-syntax? predicate/c]
  [multiline-syntax? predicate/c]))


(require rebellion/streaming/reducer
         rebellion/streaming/transducer
         resyntax/default-recommendations/private/syntax-identifier-sets)


(module+ test
  (require (submod "..")
           rackunit
           rebellion/private/static-name))


;@----------------------------------------------------------------------------------------------------


(define (oneline-syntax? v)
  (and (syntax? v) (equal? (syntax-line-count v) 1)))


(define (multiline-syntax? v)
  (and (syntax? v) (> (syntax-line-count v) 1)))


(define (syntax-line-count stx)
  (transduce (in-syntax-identifiers stx)
             (mapping syntax-line)
             (deduplicating)
             #:into into-count))


(module+ test
  (test-case (name-string oneline-syntax?)
    (check-true (oneline-syntax? #'a))
    (check-true (oneline-syntax? #'(a b c)))
    (check-false (oneline-syntax? #'(a
                                     b
                                     c))))

  (test-case (name-string multiline-syntax?)
    (check-false (multiline-syntax? #'a))
    (check-false (multiline-syntax? #'(a b c)))
    (check-true (multiline-syntax? #'(a
                                      b
                                      c)))))
