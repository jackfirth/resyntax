#lang racket/base


(require racket/contract/base)


(provide
 ORIGINAL-GAP
 (contract-out
  [syntax-replacement? predicate/c]
  [syntax-replacement
   (-> #:original-syntax (and/c syntax? syntax-original?)
       #:new-syntax syntax?
       #:introduction-scope (->* (syntax?) ((or/c 'flip 'add 'remove)) syntax?)
       syntax-replacement?)]
  [syntax-replacement-render (-> syntax-replacement? string-replacement?)]
  [syntax-replacement-original-syntax (-> syntax-replacement? (and/c syntax? syntax-original?))]
  [syntax-replacement-new-syntax (-> syntax-replacement? syntax?)]
  [syntax-replacement-preserves-free-identifiers? (-> syntax-replacement? boolean?)]
  [syntax-replacement-preserves-comments? (-> syntax-replacement? range-set? boolean?)]
  [syntax-replacement-dropped-comment-locations (-> syntax-replacement? range-set? range-set?)]))


(require (for-syntax racket/base)
         guard
         racket/format
         racket/list
         racket/match
         racket/sequence
         rebellion/base/comparator
         (only-in rebellion/base/range closed-open-range)
         rebellion/collection/range-set
         rebellion/private/static-name
         rebellion/type/record
         resyntax/private/string-replacement
         resyntax/private/syntax-neighbors
         (only-in resyntax/default-recommendations/private/syntax-identifier-sets
                  in-syntax-identifiers)
         syntax/parse)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-syntax (ORIGINAL-GAP stx)
  (raise-syntax-error
   #false
   "should only be used by refactoring rules to indicate where to insert the content between two\
 original syntax objects"
   stx))


(define-record-type syntax-replacement
  (original-syntax new-syntax introduction-scope))


(define (syntax-replacement-render replacement)

  (define/guard (pieces stx)
    (guard (not (syntax-original? stx)) #:else
      (define start (sub1 (syntax-position stx)))
      (define end (+ start (syntax-span stx)))
      (list (copied-string start end)))
    (syntax-parse stx
      #:literals (quote ORIGINAL-GAP)

      [(ORIGINAL-GAP ~! before after)
       (define before-end (+ (sub1 (syntax-position #'before)) (syntax-span #'before)))
       (define after-start (sub1 (syntax-position #'after)))
       (list (copied-string before-end after-start))]
      
      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
       (list (inserted-string (string->immutable-string (~s (syntax-e #'v)))))]
      
      [(quote datum) (cons (inserted-string "'") (pieces #'datum))]
      
      [(subform ...)
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (define subform-piece-lists
         (for/list ([subform-stx (in-list (attribute subform))]
                    [trailing-stx (in-list (shift-left (attribute subform)))])
           (define separator-piece
             (and trailing-stx
                  (or (original-separator-piece subform-stx trailing-stx) (inserted-string " "))))
           (if separator-piece
               (append (pieces subform-stx) (list separator-piece))
               (pieces subform-stx))))
       (append
        (list (inserted-string opener))
        (apply append subform-piece-lists)
        (list (inserted-string closer)))]
      
      [(subform ... . tail-form)
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (define subform-piece-lists
         (for/list ([subform-stx (in-list (attribute subform))]
                    [trailing-stx (in-list (shift-left (attribute subform)))])
           (define separator-piece
             (and trailing-stx
                  (or (original-separator-piece subform-stx trailing-stx) (inserted-string " "))))
           (if separator-piece
               (append (pieces subform-stx) (list separator-piece))
               (pieces subform-stx))))
       (define tail-pieces (pieces #'tail-form))
       (define dot-piece
         (or (original-separator-piece (last (attribute subform)) #'tail-form)
             (inserted-string " . ")))
       (append
        (list (inserted-string opener))
        (apply append subform-piece-lists)
        (list dot-piece)
        tail-pieces
        (list (inserted-string closer)))]))

  (match-define (syntax-replacement #:original-syntax orig-stx #:new-syntax new-stx) replacement)
  (define start (sub1 (syntax-position orig-stx)))
  (string-replacement
   #:start start #:end (+ start (syntax-span orig-stx)) #:contents (pieces new-stx)))


(define/guard (original-separator-piece stx trailing-stx)
  (guard (syntax-originally-neighbors? stx trailing-stx) #:else #false)
  (let* ([stx (syntax-extract-original stx)]
         [trailing-stx (syntax-extract-original trailing-stx)])
    (define stx-end (+ (sub1 (syntax-position stx)) (syntax-span stx)))
    (define trailing-start (sub1 (syntax-position trailing-stx)))
    (copied-string stx-end trailing-start)))


(define/guard (shift-left vs)
  (guard-match (cons _ shifted-vs) vs #:else '())
  (append shifted-vs (list #false)))


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
          #:new-syntax new-stx
          #:introduction-scope flip))
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


(define (syntax-replacement-preserves-free-identifiers? replacement)
  (match replacement
    [(syntax-replacement #:original-syntax orig
                         #:new-syntax new
                         #:introduction-scope intro)
     (define ignore (list #'ORIGINAL-GAP))
     (for/and ([new-id (in-syntax-identifiers new)]
               #:unless (member new-id ignore free-identifier=?)
               #:unless (bound-identifier=? new-id (intro new-id 'remove)))
       (free-identifier=? new-id (datum->syntax orig (syntax->datum new-id))))]))


(define (syntax-replacement-dropped-comment-locations replacement all-comment-locations)
  (define original-syntax-range
    (syntax-source-range (syntax-replacement-original-syntax replacement)))
  (define comment-locations (range-subset all-comment-locations original-syntax-range))
  (range-set-remove-all comment-locations (syntax-replacement-preserved-locations replacement)))


(define (syntax-replacement-preserves-comments? replacement all-comment-locations)
  (define original-syntax-range
    (syntax-source-range (syntax-replacement-original-syntax replacement)))
  (define comment-locations (range-subset all-comment-locations original-syntax-range))
  (range-set-encloses-all? (syntax-replacement-preserved-locations replacement) comment-locations))


(define (syntax-replacement-preserved-locations replacement)
  (string-replacement-preserved-locations (syntax-replacement-render replacement)))


(define (syntax-source-range stx)
  (define start (sub1 (syntax-position stx)))
  (closed-open-range start (+ start (syntax-span stx)) #:comparator natural<=>))
