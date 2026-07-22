#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [oneline-syntax? (-> any/c boolean?)]
  [multiline-syntax? (-> any/c boolean?)]))


(require resyntax/private/syntax-traversal
         syntax/parse)


(module+ test
  (require rackunit
           rebellion/private/static-name
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define (oneline-syntax? v)
  (and (syntax? v) (equal? (syntax-line-count v #:limit 2) 1)))


(define (multiline-syntax? v)
  (and (syntax? v) (> (syntax-line-count v #:limit 2) 1)))


;; Counts how many distinct source lines the atoms within stx occupy, giving up and returning limit
;; as soon as that many distinct lines have been seen. Both predicates above only need to
;; distinguish between zero, one, and multiple lines, and they're checked against enough syntax
;; objects that traversing entire subtrees after the answer is already determined shows up as a
;; large chunk of Resyntax's runtime.
(define (syntax-line-count stx #:limit limit)
  (let/ec return
    (define lines-seen '())
    (let loop ([stx stx])
      (syntax-parse stx
        [:atom
         (define line (syntax-line this-syntax))
         (unless (member line lines-seen)
           (set! lines-seen (cons line lines-seen))
           (when (>= (length lines-seen) limit)
             (return limit)))]
        [(part ...)
         #:cut
         (for ([part-stx (in-list (attribute part))])
           (loop part-stx))]
        [(part ...+ . tail-part)
         #:cut
         (for ([part-stx (in-list (attribute part))])
           (loop part-stx))
         (loop #'tail-part)]
        [_ (void)]))
    (length lines-seen)))


(module+ test
  (test-case (name-string oneline-syntax?)
    (check-true (oneline-syntax? #'a))
    (check-true (oneline-syntax? #'(a b c)))
    (check-false (oneline-syntax? #'(a
                                     b
                                     c)))
    (check-false (oneline-syntax? #'(1
                                     2
                                     3)))
    (check-true (oneline-syntax? #'(a . b)))
    (check-false (oneline-syntax? #'(a .
                                       b)))
    (check-false (oneline-syntax? 'not-syntax))

    ;; Syntax objects containing no atoms at all occupy zero lines, so they're neither one-line nor
    ;; multiline.
    (check-false (oneline-syntax? #'()))
    (check-false (multiline-syntax? #'())))

  (test-case (name-string multiline-syntax?)
    (check-false (multiline-syntax? #'a))
    (check-false (multiline-syntax? #'(a b c)))
    (check-true (multiline-syntax? #'(a
                                      b
                                      c)))
    (check-true (multiline-syntax? #'(1
                                      2
                                      3)))
    (check-false (multiline-syntax? #'(a . b)))
    (check-true (multiline-syntax? #'(a .
                                        b)))
    (check-false (multiline-syntax? 'not-syntax))))
