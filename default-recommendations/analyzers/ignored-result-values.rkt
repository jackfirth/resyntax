#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [ignored-result-values-analyzer expansion-analyzer?]))


(require racket/stream
         resyntax/private/analyzer
         resyntax/private/syntax-path
         resyntax/private/syntax-property-bundle
         resyntax/private/syntax-traversal
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define (ignored-result-values stx)
  (let loop ([stx (syntax-label-paths stx 'expansion-path)])

    (define (mark-result stx mode)
      (define path (syntax-property stx 'expansion-path))
      (stream-cons (syntax-property-entry path 'expression-result mode) (loop stx)))

    (define (mark-all stxs mode)
      (stream-append-all
       (for/list ([stx (in-list stxs)])
         (mark-result stx mode))))

    (syntax-search stx
      #:literal-sets (kernel-literals)

      [((~or define-values define-syntaxes) _ expr)
       (mark-result (attribute expr) 'used)]

      [(#%plain-lambda _ body ... result)
       (stream-append
        (mark-all (attribute body) 'ignored)
        (mark-result (attribute result) 'used))]

      [(case-lambda [_ body ... result] ...)
       (stream-append-all
        (for/list ([bodies (in-list (attribute body))]
                   [result-stx (in-list (attribute result))])
          (stream-append (mark-all bodies 'ignored)
                         (mark-result result-stx 'used))))]

      [(if condition true-branch false-branch)
       (stream-append
        (mark-result (attribute condition) 'used)
        (mark-result (attribute true-branch) 'used)
        (mark-result (attribute false-branch) 'used))]

      [(begin body ... result)
       (stream-append
        (mark-all (attribute body) 'ignored)
        (mark-result (attribute result) 'used))]

      [(begin0 result body ...)
       (stream-append (mark-result (attribute result) 'used)
                      (mark-all (attribute body) 'ignored))]

      [((~or let-values letrec-values) ([_ expr] ...)
         body ...
         result)
       (stream-append (mark-all (attribute expr) 'used)
                      (mark-all (attribute body) 'ignored)
                      (mark-result (attribute result) 'used))]

      [(set! _ expr) (mark-result (attribute expr) 'used)]

      [(with-continuation-mark key val result)
       (stream-append
        (mark-result (attribute key) 'used)
        (mark-result (attribute val) 'used)
        (mark-result (attribute result) 'used))]

      [(#%plain-app func-or-arg ...) (mark-all (attribute func-or-arg) 'used)])))


(define ignored-result-values-analyzer
  (make-expansion-analyzer
   (λ (stx) (sequence->syntax-property-bundle (ignored-result-values stx)))
   #:name 'ignored-result-values-analyzer))


(define (stream-append-all streams)
  (apply stream-append streams))


(module+ test
  (test-case "ignored-result-values-analyzer"
    (define test-stx
      #'(define (f x)
          (displayln "hi")
          x))
    (define expanded (expand test-stx))
    (define lambda-path (syntax-path (list 2)))
    (define displayln-path (syntax-path (list 2 2)))
    (define displayln-id-path (syntax-path (list 2 2 (tail-syntax 1) 0)))
    (define hi-path (syntax-path (list 2 2 (tail-syntax 1) 1)))
    (define x-path (syntax-path (list 2 3)))
    (check-equal? (syntax->datum (syntax-ref expanded lambda-path))
                  '(lambda (x) (#%app displayln '"hi") x))
    (check-equal? (syntax->datum (syntax-ref expanded displayln-path)) '(#%app displayln '"hi"))
    (check-equal? (syntax->datum (syntax-ref expanded displayln-id-path)) 'displayln)
    (check-equal? (syntax->datum (syntax-ref expanded hi-path)) ''"hi")
    (check-equal? (syntax->datum (syntax-ref expanded x-path)) 'x)

    (define actual (expansion-analyze ignored-result-values-analyzer expanded))

    (define expected
      (syntax-property-bundle
       (syntax-property-entry lambda-path 'expression-result 'used)
       (syntax-property-entry displayln-path 'expression-result 'ignored)
       (syntax-property-entry displayln-id-path 'expression-result 'used)
       (syntax-property-entry hi-path 'expression-result 'used)
       (syntax-property-entry x-path 'expression-result 'used)))
    (check-equal? actual expected)))
