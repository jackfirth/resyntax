#lang racket/base

(require racket/contract/base)

(provide
 NEWLINE
 (contract-out
  [syntax-replacement? predicate/c]
  [syntax-replacement
   (-> #:original-syntax (and/c syntax? syntax-original?)
       #:new-syntax syntax?
       #:source source-code?
       #:code-string immutable-string?
       syntax-replacement?)]
  [syntax-replacement-render (-> syntax-replacement? immutable-string?)]
  [syntax-replacement-original-syntax (-> syntax-replacement? (and/c syntax? syntax-original?))]
  [syntax-replacement-new-syntax (-> syntax-replacement? syntax?)]
  [syntax-replacement-source (-> syntax-replacement? source-code?)]
  [syntax-replacement-code-string (-> syntax-replacement? immutable-string?)]
  [source-range? predicate/c]
  [source-range (-> natural? natural? source-range?)]
  [source-range-start (-> source-range? natural?)]
  [source-range-end (-> source-range? natural?)]
  [indent-code (-> immutable-string? (sequence/c source-range?) immutable-string?)]))

(require (for-syntax racket/base)
         framework
         racket/class
         racket/format
         racket/match
         racket/math
         racket/sequence
         racket/string
         rebellion/base/immutable-string
         rebellion/private/guarded-block
         rebellion/type/record
         rebellion/type/tuple
         resyntax/source-code
         syntax/parse)

(module+ test
  (require (submod "..")
           rackunit))

;@----------------------------------------------------------------------------------------------------

(define-syntax (NEWLINE stx)
  (raise-syntax-error
   'NEWLINE
   "should only be used by refactoring rules to indicate where newlines should be inserted"
   stx))

(define-record-type syntax-replacement (original-syntax new-syntax source code-string))

(define/guard (syntax-replacement-render replacement)
  (match-define
    (syntax-replacement
     #:original-syntax orig-stx #:new-syntax new-stx #:source source #:code-string code-string)
    replacement)
  (define/guard (initial-replacement stx)
    (guard (source-code-produced-syntax? source stx) then
      (define start (sub1 (syntax-position stx)))
      (define end (+ start (syntax-span stx)))
      (string->immutable-string (substring code-string start end)))
    (syntax-parse stx
      #:literals (quote NEWLINE)
      [NEWLINE "\n"]
      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
       (string->immutable-string (~s (syntax-e #'v)))]
      [(quote datum) (string->immutable-string (string-append "'" (initial-replacement #'datum)))]
      [(subform ...)
       (define subform-strings
         (for/list ([subform-stx (in-syntax #'(subform ...))]) (initial-replacement subform-stx)))
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (string->immutable-string
        (string-join subform-strings #:before-first opener #:after-last closer))]))
  (initial-replacement new-stx))

(define-tuple-type source-range (start end))

(define (indent-code code-string source-ranges)
  (define text-object (new racket:text%))
  (send text-object insert code-string)
  (for/fold ([position-skew 0] #:result (void))
            ([range source-ranges])
    (match-define (source-range start end) range)
    (define skewed-start (+ start position-skew))
    (define skewed-end (+ end position-skew))
    (send text-object set-position skewed-start skewed-end)
    (send text-object tabify-selection)
    (define indented-end (send text-object get-end-position))
    (- indented-end skewed-end))
  (string->immutable-string (send text-object get-text)))

(module+ test
  (test-case "indent-code"
    (check-equal?
     (indent-code
      "#lang racket/base\n\n(+ 1\n2\n3)\n\n(+ 4\n5\n6)\n"
      (list (source-range 19 28) (source-range 30 39)))
     "#lang racket/base\n\n(+ 1\n   2\n   3)\n\n(+ 4\n   5\n   6)\n")
    (check-equal?
     (indent-code
      "#lang racket/base\n\n(+ 1\n2\n3)\n\n(+ 4\n5\n6)\n"
      (list (source-range 19 28)))
     "#lang racket/base\n\n(+ 1\n   2\n   3)\n\n(+ 4\n5\n6)\n")
    (check-equal?
     (indent-code
      "#lang racket/base\n\n(+ 1\n2\n3)\n\n(+ 4\n5\n6)\n"
      (list (source-range 30 39)))
     "#lang racket/base\n\n(+ 1\n2\n3)\n\n(+ 4\n   5\n   6)\n")))
