#lang racket/base

(require racket/contract/base)

(provide (contract-out [limiting (-> (>=/c 0) #:by (-> any/c (>=/c 0)) transducer?)]))

(require rebellion/base/variant
         rebellion/streaming/transducer)

(module+ test
  (require racket/match
           rackunit
           rebellion/collection/list
           (submod "..")))

;@----------------------------------------------------------------------------------------------------

(struct limiting-emit-state (weight-so-far element) #:transparent)

(define (limiting max-weight #:by weight-function)

  (define (start)
    (variant #:consume 0))

  (define (consume weight-so-far v)
    (define new-weight (+ weight-so-far (weight-function v)))
    (cond
      [(< new-weight max-weight) (variant #:emit (limiting-emit-state new-weight v))]
      [(> new-weight max-weight) (variant #:consume weight-so-far)]
      [else (variant #:half-closed-emit v)]))

  (define (emit s)
    (define weight-so-far (limiting-emit-state-weight-so-far s))

    (emission (variant #:consume weight-so-far) (limiting-emit-state-element s)))

  (define (half-closed-emit v)
    (half-closed-emission (variant #:finish #false) v))

  (make-transducer #:starter start
                   #:consumer consume
                   #:emitter emit
                   #:half-closer (λ (_) (variant #:finish #false))
                   #:half-closed-emitter half-closed-emit
                   #:finisher void
                   #:name 'limiting))

(module+ test
  (test-case "limiting"
    (define inputs (list 'small 'big 'medium 'medium 'big 'small))

    (define (weight i)
      (match i
        ['small 1]
        ['medium 5]
        ['big 10]))

    (check-equal? (transduce inputs (limiting 3 #:by weight) #:into into-list) (list 'small 'small))
    (check-equal? (transduce inputs (limiting 13 #:by weight) #:into into-list)
                  (list 'small 'big 'small))
    (check-equal? (transduce inputs (limiting 16 #:by weight) #:into into-list)
                  (list 'small 'big 'medium))
    (check-equal? (transduce inputs (limiting +inf.0 #:by weight) #:into into-list)
                  (list 'small 'big 'medium 'medium 'big 'small))
    (check-equal? (transduce (list 'small 'big 'small 'big 'small 'big 'small 'big 'small 'big)
                             (limiting 3 #:by weight)
                             #:into into-list)
                  (list 'small 'small 'small))
    (check-equal? (transduce (list 'small 'big 'small 'big 'small 'big 'small 'big 'small 'big)
                             (limiting 5 #:by weight)
                             #:into into-list)
                  (list 'small 'small 'small 'small 'small))))
