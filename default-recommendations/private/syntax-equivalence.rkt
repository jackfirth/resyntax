#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-free-identifier=? (-> syntax? syntax? boolean?)]))


(require guard
         racket/list
         racket/match)


(module+ test
  (require rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define (syntax-free-identifier=? stx other-stx)
  (define datum (syntax-e stx))
  (define other-datum (syntax-e other-stx))
  (match datum
    [(? symbol?) (and (symbol? other-datum) (free-identifier=? stx other-stx))]
    [(? number? boolean? string?) (equal? datum other-datum)]
    [(? list?) (and (list? other-datum) (syntax-pair-free-identifier=? datum other-datum))]
    [(? box?) (and (box? other-datum) (syntax-free-identifier=? (unbox datum) (unbox other-datum)))]
    [(? vector?)
     (and (equal? (vector-length datum) (vector-length other-datum))
          (for/and ([substx (in-vector datum)] [other-substx (in-vector other-datum)])
            (syntax-free-identifier=? substx other-substx)))]
    [_ (error 'syntax-free-identifier=? "hash datum comparisons not implemented yet.")]))


(define (syntax-pair-free-identifier=? pair other-pair)
  (match pair
    ['() (empty? other-pair)]
    [(cons head (? syntax? tail))
     (match other-pair
       [(cons other-head (? syntax? other-tail))
        (and (syntax-free-identifier=? head other-head)
             (syntax-free-identifier=? tail other-tail))]
       [_ #false])]
    [(cons head tail)
     (match other-pair
       [(cons other-head other-tail)
        (and (syntax-free-identifier=? head other-head)
             (syntax-pair-free-identifier=? tail other-tail))]
       [_ #false])]))


(module+ test
  (test-case "syntax-free-identifier=?"
    (check-true (syntax-free-identifier=? #'5 #'5))
    (check-false (syntax-free-identifier=? #'5 #'7))
    (check-false (syntax-free-identifier=? #'5 #'x))
    (check-true (syntax-free-identifier=? #'x #'x))
    (check-true (syntax-free-identifier=? #'(x y z) #'(x y z)))
    (check-false (syntax-free-identifier=? #'(x y z) #'(x y)))
    (check-true (syntax-free-identifier=? #'(x (y) z) #'(x (y) z)))
    (check-false (syntax-free-identifier=? #'(x (y) z) #'(x y z)))))
