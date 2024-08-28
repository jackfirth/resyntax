#lang racket/base


(require racket/contract/base)


(provide
 ORIGINAL-GAP
 ORIGINAL-SPLICE
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
  [syntax-replacement-preserves-comments? (-> syntax-replacement? range-set? boolean?)]))


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


(define-syntax (ORIGINAL-SPLICE stx)
  (raise-syntax-error
   #false
   "should only be used by refactoring rules to indicate original sequences of syntax objects"
   stx))


(define-record-type syntax-replacement
  (original-syntax new-syntax introduction-scope))


(define (template-separator? stx)
  (syntax-parse stx
    #:literals (ORIGINAL-GAP)
    [(ORIGINAL-GAP _ ...) #true]
    [else #false]))


(define/guard (add-contents-between lst adder)
  (guard-match (cons first-element remaining-elements) lst #:else '())
  (cons
   first-element
   (for/list ([previous (in-list lst)]
              [element (in-list remaining-elements)]
              #:when #true
              [inserted (append (adder previous element) (list element))])
     inserted)))


(module+ test
  (test-case (name-string add-contents-between)

    (define (appended-strings left right)
      (list (format "left: ~a" left) (format "right: ~a" right)))
    
    (test-case "empty list"
      (check-equal? (add-contents-between '() appended-strings) '()))

    (test-case "singleton list"
      (check-equal? (add-contents-between (list 1) appended-strings) (list 1)))

    (test-case "two-element list"
      (define actual (add-contents-between (list 1 2) appended-strings))
      (check-equal? actual (list 1 "left: 1" "right: 2" 2)))

    (test-case "many-element list"
      (define actual (add-contents-between (list 1 2 3) appended-strings))
      (check-equal? actual (list 1 "left: 1" "right: 2" 2 "left: 2" "right: 3" 3)))))


(define (syntax-replacement-render replacement)

  (define/guard (pieces stx)
    (guard (not (syntax-original? stx)) #:else
      (define start (sub1 (syntax-position stx)))
      (define end (+ start (syntax-span stx)))
      (list (copied-string start end)))
    (syntax-parse stx
      #:literals (quote ORIGINAL-GAP ORIGINAL-SPLICE)

      [(ORIGINAL-GAP ~! before after)
       (define before-end (+ (sub1 (syntax-position #'before)) (syntax-span #'before)))
       (define after-start (sub1 (syntax-position #'after)))
       (list (copied-string before-end after-start))]

      [(ORIGINAL-SPLICE ~! original-subform ...)
       (guarded-block
         (define subforms (syntax->list #'(original-subform ...)))
         (guard (not (empty? subforms)) #:else (list))
         (for ([subform-stx (in-list subforms)])
           (unless (syntax-original? subform-stx)
             (raise-arguments-error
              (name syntax-replacement-render)
              "replacement subform within an ORIGINAL-SPLICE form is not original syntax"
              "subform" subform-stx
              "splice" stx)))
         (define start (sub1 (syntax-position (first subforms))))
         (define end (+ (sub1 (syntax-position (last subforms))) (syntax-span (last subforms))))
         (list (copied-string start end)))]
      
      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
       (list (inserted-string (string->immutable-string (~s (syntax-e #'v)))))]
      
      [(quote datum) (cons (inserted-string "'") (pieces #'datum))]
      
      [(subform ...)
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (define subform-piece-lists
         (for/list ([subform-stx (in-list (attribute subform))])
           (pieces subform-stx)))
       (append
        (list (inserted-string opener))
        (join-piece-lists subform-piece-lists)
        (list (inserted-string closer)))]
      
      [(subform ... . tail-form)
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (define subform-pieces
         (join-piece-lists
          (for/list ([subform-stx (in-syntax #'(subform ...))]) (pieces subform-stx))))
       (define tail-pieces (pieces #'tail-form))
       (define dot-string " . ")
       (append
        (list (inserted-string opener))
        subform-pieces
        (list (inserted-string dot-string))
        tail-pieces
        (list (inserted-string closer)))]))

  (match-define (syntax-replacement #:original-syntax orig-stx #:new-syntax new-stx) replacement)
  (define start (sub1 (syntax-position orig-stx)))
  (string-replacement
   #:start start #:end (+ start (syntax-span orig-stx)) #:contents (pieces new-stx)))


(define/guard (ends-with-newline? piece-list)
  (guard (not (empty? piece-list)) #:else #true)
  (define last-piece (last piece-list))
  (guard (inserted-string? last-piece) #:else #false)
  (define str (inserted-string-contents last-piece))
  (equal? (string-ref str (sub1 (string-length str))) #\newline))


(define/guard (starts-with-newline? piece-list)
  (guard (not (empty? piece-list)) #:else #true)
  (define first-piece (first piece-list))
  (guard (inserted-string? first-piece) #:else #false)
  (define str (inserted-string-contents first-piece))
  (equal? (string-ref str 0) #\newline))


(define/guard (join-piece-lists piece-lists)
  (guard (not (empty? piece-lists)) #:else '())
  (apply append (add-between piece-lists (list (inserted-string " ")))))


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
     (define ignore (list #'ORIGINAL-GAP #'ORIGINAL-SPLICE))
     (for/and ([new-id (in-syntax-identifiers new)]
               #:unless (member new-id ignore free-identifier=?)
               #:unless (bound-identifier=? new-id (intro new-id 'remove)))
       (free-identifier=? new-id (datum->syntax orig (syntax->datum new-id))))]))


(define (syntax-replacement-preserves-comments? replacement all-comment-locations)
  (define original-syntax-range
    (syntax-source-range (syntax-replacement-original-syntax replacement)))
  (define comment-locations (range-subset all-comment-locations original-syntax-range))
  (range-set-encloses-all? (syntax-replacement-preserved-locations replacement) comment-locations))


(define (syntax-replacement-preserved-locations replacement)
  (define stx (syntax-replacement-new-syntax replacement))

  (define/guard (pieces stx)
    (guard (not (syntax-original? stx)) #:else (list (syntax-source-range stx)))
    (syntax-parse stx
      #:literals (quote ORIGINAL-GAP ORIGINAL-SPLICE)

      [(ORIGINAL-GAP ~! before after)
       (define before-end (+ (sub1 (syntax-position #'before)) (syntax-span #'before)))
       (define after-start (sub1 (syntax-position #'after)))
       (list (closed-open-range before-end after-start #:comparator natural<=>))]
      
      [(ORIGINAL-SPLICE ~! original-subform ...)
       (guarded-block
         (define subforms (syntax->list #'(original-subform ...)))
         (guard (not (empty? subforms)) #:else (list))
         (for ([subform-stx (in-list subforms)])
           (unless (syntax-original? subform-stx)
             (raise-arguments-error
              (name syntax-replacement-render)
              "replacement subform within an ORIGINAL-SPLICE form is not original syntax"
              "subform" subform-stx
              "splice" stx)))
         (define start (sub1 (syntax-position (first subforms))))
         (define end (+ (sub1 (syntax-position (last subforms))) (syntax-span (last subforms))))
         (list (closed-open-range start end #:comparator natural<=>)))]
      
      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes) (list)]
      
      [(quote datum) (pieces #'datum)]
      
      [(subform ...) (append-map pieces (syntax->list #'(subform ...)))]
      
      [(subform ... . tail-form) (append-map pieces (syntax->list #'(subform ... tail-form)))]))
  
  (sequence->range-set (pieces stx) #:comparator natural<=>))


(define (syntax-source-range stx)
  (define start (sub1 (syntax-position stx)))
  (closed-open-range start (+ start (syntax-span stx)) #:comparator natural<=>))
