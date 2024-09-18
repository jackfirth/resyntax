#lang racket/base


(provide ~if)


(require syntax/parse
         syntax/parse/experimental/template)


(module+ test
  (require racket/syntax
           rackunit
           rebellion/private/static-name
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-template-metafunction (~if stx)
  (syntax-parse stx
    #:track-literals
    [(_ condition true false)
     (if (syntax-e #'condition) #'true #'false)]))


(module+ test
  (test-case (name-string ~if)
    (define/with-syntax (condition ...) #'(#true #false #false))
    (define/with-syntax a #'foo)
    (define/with-syntax b #'bar)
    (define stx #'((~if condition a b) ...))
    (check-equal? (syntax->datum stx) '(foo bar bar))))
