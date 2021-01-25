#lang racket/base


(require racket/contract/base)


(provide
 NEWLINE
 (contract-out
  [syntax-replacement? predicate/c]
  [syntax-replacement
   (-> #:original-syntax (and/c syntax? syntax-original?) #:new-syntax syntax? syntax-replacement?)]
  [syntax-replacement-render (-> syntax-replacement? string-replacement?)]
  [syntax-replacement-original-syntax (-> syntax-replacement? (and/c syntax? syntax-original?))]
  [syntax-replacement-new-syntax (-> syntax-replacement? syntax?)]))


(require (for-syntax racket/base)
         racket/format
         racket/list
         racket/match
         racket/sequence
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/type/record
         resyntax/string-replacement
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
       (append
        (list (inserted-string opener))
        (join-piece-lists (for/list ([subform-stx (in-syntax #'(subform ...))]) (pieces subform-stx)))
        (list (inserted-string closer)))]))

  (match-define (syntax-replacement #:original-syntax orig-stx #:new-syntax new-stx) replacement)
  (define start (sub1 (syntax-position orig-stx)))
  (string-replacement
   #:start start #:end (+ start (syntax-span orig-stx)) #:contents (pieces new-stx)))


(define/guard (ends-with-newline? piece-list)
  (guard (empty? piece-list) then #true)
  (define last-piece (last piece-list))
  (guard (inserted-string? last-piece) else #false)
  (define str (inserted-string-contents last-piece))
  (equal? (string-ref str (sub1 (string-length str))) #\newline))


(define/guard (starts-with-newline? piece-list)
  (guard (empty? piece-list) then #true)
  (define first-piece (first piece-list))
  (guard (inserted-string? first-piece) else #false)
  (define str (inserted-string-contents first-piece))
  (equal? (string-ref str 0) #\newline))


(define/guard (join-piece-lists piece-lists)
  (guard (empty? piece-lists) then '())
  (append
   (for/list ([piece-list (in-list piece-lists)]
              [next-piece-list (in-list (rest piece-lists))]
              #:when #true
              [piece
               (in-list
                (if (or (ends-with-newline? piece-list) (starts-with-newline? next-piece-list))
                    piece-list
                    (append piece-list (list (inserted-string " ")))))])
     piece)
   (last piece-lists)))


(module+ test
  (test-case (name-string syntax-replacement-render)
    (define flip (make-syntax-introducer))
    (define orig-stx #'(+ 1 (+ 2 3)))
    (cond
      [(not (syntax-original? orig-stx))
       (displayln
        "skipping syntax-replacement-render test because orignal-ness is lost in compiled code")]
      [else
       (define orig-start (sub1 (syntax-position orig-stx)))
       (define new-stx
         (flip
          (syntax-parse (flip orig-stx)
            #:literals (+)
            [((~and + +_1) x (+ y z)) #'(+_1 x y z)])))
       (define replacement
         (syntax-replacement
          #:original-syntax orig-stx
          #:new-syntax new-stx))
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
       (check-equal? (syntax-replacement-render replacement) expected)])))
