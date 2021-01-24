#lang racket/base


(require racket/contract/base)


(provide
 NEWLINE
 (contract-out
  [syntax-replacement? predicate/c]
  [syntax-replacement
   (-> #:original-syntax (and/c syntax? syntax-original?) #:new-syntax syntax? syntax-replacement?)]
  [syntax-replacement-render (-> syntax-replacement? string-replacement?)]
  [syntax-replacement-render-string
   (-> syntax-replacement? #:code-string immutable-string? immutable-string?)]
  [syntax-replacement-original-syntax (-> syntax-replacement? (and/c syntax? syntax-original?))]
  [syntax-replacement-new-syntax (-> syntax-replacement? syntax?)]
  [source-range? predicate/c]
  [source-range (-> natural? natural? source-range?)]
  [source-range-start (-> source-range? natural?)]
  [source-range-end (-> source-range? natural?)]
  [indent-code (-> immutable-string? (sequence/c source-range?) immutable-string?)]))


(require (for-syntax racket/base)
         framework
         racket/class
         racket/format
         racket/list
         racket/match
         racket/math
         racket/sequence
         racket/string
         rebellion/base/immutable-string
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/type/record
         rebellion/type/tuple
         resyntax/source-code
         resyntax/string-replacement
         syntax/parse)


(module+ test
  (require (submod "..")
           racket/port
           rackunit
           syntax/modread))


;@----------------------------------------------------------------------------------------------------


(define-syntax (NEWLINE stx)
  (raise-syntax-error
   'NEWLINE
   "should only be used by refactoring rules to indicate where newlines should be inserted"
   stx))


(define-record-type syntax-replacement (original-syntax new-syntax))


(define/guard (syntax-replacement-render replacement)

  (define/guard (pieces stx)
    (guard (syntax-original? stx) then
      (define start (sub1 (syntax-position stx)))
      (define end (+ start (syntax-span stx)))
      (list (copied-string start end)))
    (syntax-parse stx
      #:literals (quote NEWLINE)
      [NEWLINE (list (inserted-string "\n"))]
      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
       (list (inserted-string (string->immutable-string (~s (syntax-e #'v)))))]
      [(quote datum) (cons (inserted-string "'") (pieces #'datum))]
      [(subform ...)
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (define subform-piece-lists
         (for/list ([subform-stx (in-syntax #'(subform ...))]) (pieces subform-stx)))
       (append*
        (add-between
         subform-piece-lists
         (list (list (inserted-string " ")))
         #:before-first (list (list (inserted-string opener)))
         #:after-last (list (list (inserted-string closer)))
         #:splice? #true))]))

  (match-define (syntax-replacement #:original-syntax orig-stx #:new-syntax new-stx) replacement)
  (define start (sub1 (syntax-position orig-stx)))
  (string-replacement
   #:start start #:end (+ start (syntax-span orig-stx)) #:contents (pieces new-stx)))


(define/guard (syntax-replacement-render-string replacement #:code-string code-string)
  (string-replacement-content (syntax-replacement-render replacement) code-string))


(module+ test
  (test-case (name-string syntax-replacement-render)
    (define flip (make-syntax-introducer))
    (define orig-stx #'(+ 1 (+ 2 3)))
    (define orig-start (sub1 (syntax-position orig-stx)))
    (define new-stx
      (flip
       (syntax-parse (flip orig-stx)
         #:literals (+)
         [((~and + +_1) x (+ y z)) #'(+_1 x y z)])))
    (define replacement
      (syntax-replacement
       #:original-syntax orig-stx
       #:new-syntax new-stx
       #:source (string-source-code "(+ 1 (+ 2 3))")
       #:code-string "(+ 1 (+ 2 3))"))
    (define expected
      (string-replacement
       #:start orig-start
       #:end (+ orig-start 13)
       #:contents
       (list
        (inserted-string "(")
        (copied-string (+ orig-start 1) (+ orig-start 2))
        (inserted-string " ")
        (copied-string (+ orig-start 3) (+ orig-start 4))
        (inserted-string " ")
        (copied-string (+ orig-start 8) (+ orig-start 9))
        (inserted-string " ")
        (copied-string (+ orig-start 10) (+ orig-start 11))
        (inserted-string ")"))))
    (check-equal? (syntax-replacement-render replacement) expected)))


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
