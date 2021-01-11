#lang racket/base

(require racket/contract/base)

(provide
 (contract-out
  [syntax-render (-> syntax? source-code? immutable-string? immutable-string?)]))

(require racket/format
         racket/sequence
         racket/string
         rebellion/base/immutable-string
         rebellion/private/guarded-block
         resyntax/source-code
         syntax/parse)

;@----------------------------------------------------------------------------------------------------

(define/guard (syntax-render stx source code-string)
  (guard (source-code-produced-syntax? source stx) then
    (define start (sub1 (syntax-position stx)))
    (define end (+ start (syntax-span stx)))
    (string->immutable-string (substring code-string start end)))
  (define (recur stx)
    (syntax-render stx source code-string))
  (syntax-parse stx
    #:literals (quote)
    [id:id (symbol->immutable-string (syntax-e #'id))]
    [(~or v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
     (string->immutable-string (~s (syntax-e #'v)))]
    [(quote datum) (string->immutable-string (string-append "'" (recur #'datum)))]
    [(subform ...)
     (define subform-strings
       (for/list ([subform-stx (in-syntax #'(subform ...))]) (recur subform-stx)))
     (string->immutable-string (string-join subform-strings #:before-first "(" #:after-last ")"))]))
