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
