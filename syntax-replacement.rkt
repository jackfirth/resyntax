#lang debug racket/base


(require racket/contract/base)


(provide
 SPACE
 NEWLINE
 ORIGINAL-GAP
 ORIGINAL-SPLICE
 (contract-out
  [syntax-replacement? predicate/c]
  [syntax-replacement
   (-> #:original-syntax (and/c syntax? syntax-original?) #:new-syntax syntax? syntax-replacement?)]
  [syntax-replacement-render (-> syntax-replacement? string-replacement?)]
  [syntax-replacement-original-syntax (-> syntax-replacement? (and/c syntax? syntax-original?))]
  [syntax-replacement-new-syntax (-> syntax-replacement? syntax?)]
  [syntax-replacement-template-drop-leading-newline (-> syntax? syntax?)]))


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
   #false
   "should only be used by refactoring rules to indicate where newlines should be inserted"
   stx))


(define-syntax (SPACE stx)
  (raise-syntax-error
   #false
   "should only be used by refactoring rules to indicate where a space should be inserted"
   stx))


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


(define (syntax-replacement-template-drop-leading-newline template-stx)
  (syntax-parse template-stx
    #:literals (NEWLINE)
    [(NEWLINE form ...) #'(form ...)]
    [_ template-stx]))


(define-record-type syntax-replacement (original-syntax new-syntax))


(define/guard (syntax-replacement-template-infer-spaces template)
  (define/guard (loop template)
    (guard (syntax-original? template) then
      template)
    (syntax-parse template
      #:literals (quote NEWLINE SPACE ORIGINAL-GAP ORIGINAL-SPLICE)
      [(~or (ORIGINAL-GAP _ ...) (ORIGINAL-SPLICE _ ...) (quote _ ...)) template]
      [(subform ...)
       (define (contents-to-add-between left-form right-form)
         (if (or (template-separator? left-form) (template-separator? right-form))
             '()
             (list #'SPACE)))
       (define subforms-with-spaces
         (for/list ([subform-stx (in-syntax #'(subform ...))])
           (loop subform-stx)))
       (datum->syntax #false (add-contents-between subforms-with-spaces contents-to-add-between))]
      [else template]))
  (define flip-fresh-scope (make-syntax-introducer))
  (flip-fresh-scope (loop (flip-fresh-scope template))))


(define (template-separator? stx)
  (syntax-parse stx
    #:literals (NEWLINE SPACE ORIGINAL-GAP)
    [(~or NEWLINE SPACE (ORIGINAL-GAP _ ...)) #true]
    [else #false]))


(define/guard (add-contents-between lst adder)
  (guard (empty? lst) then
    '())
  (define first-element (first lst))
  (cons
   first-element
   (for/list ([previous (in-list lst)]
              [element (in-list (rest lst))]
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


(define/guard (syntax-replacement-render replacement)

  (define/guard (pieces stx)
    (guard (syntax-original? stx) then
      (define start (sub1 (syntax-position stx)))
      (define end (+ start (syntax-span stx)))
      (list (copied-string start end)))
    (syntax-parse stx
      #:literals (quote SPACE NEWLINE ORIGINAL-GAP ORIGINAL-SPLICE)

      [SPACE (list (inserted-string " "))]
      
      [NEWLINE (list (inserted-string "\n"))]

      [(ORIGINAL-GAP ~! before after)
       (define before-end (+ (sub1 (syntax-position #'before)) (syntax-span #'before)))
       (define after-start (sub1 (syntax-position #'after)))
       (list (copied-string before-end after-start))]
      
      [(ORIGINAL-SPLICE ~! original-subform ...+)
       (define subforms (syntax->list #'(original-subform ...)))
       (for ([subform-stx (in-list subforms)])
         (unless (syntax-original? subform-stx)
           (raise-arguments-error
            (name syntax-replacement-render)
            "replacement subform within an ORIGINAL-SPLICE form is not original syntax"
            "subform" subform-stx
            "splice" stx)))
       (define start (sub1 (syntax-position (first subforms))))
       (define end (+ (sub1 (syntax-position (last subforms))) (syntax-span (last subforms))))
       (list (copied-string start end))]
      
      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
       (list (inserted-string (string->immutable-string (~s (syntax-e #'v)))))]
      
      [(quote datum) (cons (inserted-string "'") (pieces #'datum))]
      
      [(subform ...)
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (append
        (list (inserted-string opener))
        (for*/list ([subform-stx (in-syntax #'(subform ...))]
                    [piece (in-list (pieces subform-stx))])
          piece)
        (list (inserted-string closer)))]
      
      [(subform ... . tail-form)
       (define shape (syntax-property stx 'paren-shape))
       (define opener (match shape [#false "("] [#\[ "["] [#\{ "{"]))
       (define closer (match shape [#false ")"] [#\[ "]"] [#\{ "}"]))
       (define subform-pieces
         (join-piece-lists
          (for/list ([subform-stx (in-syntax #'(subform ...))]) (pieces subform-stx))))
       (define tail-pieces (pieces #'tail-form))
       (define dot-string
         (cond
           [(and (ends-with-newline? subform-pieces) (starts-with-newline? tail-pieces)) "."]
           [(ends-with-newline? subform-pieces) ". "]
           [(starts-with-newline? tail-pieces) " ."]
           [else " . "]))
       (append
        (list (inserted-string opener))
        subform-pieces
        (list (inserted-string dot-string))
        tail-pieces
        (list (inserted-string closer)))]))

  (match-define (syntax-replacement #:original-syntax orig-stx #:new-syntax new-stx) replacement)
  (define template (syntax-replacement-template-infer-spaces new-stx))
  (define start (sub1 (syntax-position orig-stx)))
  (string-replacement
   #:start start #:end (+ start (syntax-span orig-stx)) #:contents (pieces template)))


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
