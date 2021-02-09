#lang racket/base

;; A string replacement is an operation that can be applied to a string which can insert, delete, and
;; move around the contents of the string.


(require racket/contract/base)


(provide
 (contract-out
  [string-replacement? predicate/c]
  [string-replacement
   (->i
    #:chaperone
    (#:start [start natural?]
     #:end [end natural?]
     #:contents [contents (sequence/c (or/c inserted-string? copied-string?))])
    #:pre/name (start end) "end cannot be before start" (<= start end)
    [_ string-replacement?])]
  [string-replacement-start (-> string-replacement? natural?)]
  [string-replacement-end (-> string-replacement? natural?)]
  [string-replacement-span (-> string-replacement? natural?)]
  [string-replacement-new-end (-> string-replacement? natural?)]
  [string-replacement-new-span (-> string-replacement? natural?)]
  [string-replacement-contents
   (-> string-replacement? (listof (or/c inserted-string? copied-string?)))]
  [string-replacement-overlaps? (-> string-replacement? string-replacement? boolean?)]
  [string-replacement-union
   (->i ([replacement1 string-replacement?]
         [replacement2 string-replacement?])
        #:pre/name (replacement1 replacement2)
        "replacements must not overlap"
        (not (string-replacement-overlaps? replacement1 replacement2))
        [_ string-replacement?])]
  [union-into-string-replacement (reducer/c string-replacement? string-replacement?)]
  [string-replacement-render (-> string-replacement? immutable-string? immutable-string?)]
  [string-apply-replacement (-> immutable-string? string-replacement? immutable-string?)]
  [file-apply-string-replacement! (-> path-string? string-replacement? void?)]
  [inserted-string? predicate/c]
  [inserted-string (-> immutable-string? inserted-string?)]
  [inserted-string-contents (-> inserted-string? immutable-string?)]
  [copied-string? predicate/c]
  [copied-string
   (->i
    #:chaperone
    ([start natural?]
     [end natural?])
    #:pre/name (start end) "end cannot be before start" (<= start end)
    [_ copied-string?])]
  [copied-string-start (-> copied-string? natural?)]
  [copied-string-end (-> copied-string? natural?)]))


(require racket/file
         racket/match
         racket/math
         racket/sequence
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-record-type string-replacement (start end span new-end new-span required-length contents)
  #:omit-root-binding)


(define (string-replacement #:start start #:end end #:contents contents)
  (define content-list
    (transduce contents (filtering (λ (r) (positive? (replacement-string-span r)))) #:into into-list))
  (define new-span (transduce content-list (mapping replacement-string-span) #:into into-sum))
  (define max-end
    (transduce content-list
               (filtering copied-string?)
               (mapping copied-string-end)
               #:into (into-max)))
  (constructor:string-replacement
   #:start start
   #:end end
   #:span (- end start)
   #:new-end (+ start new-span)
   #:new-span new-span
   #:required-length (add1 (option-get max-end end))
   #:contents content-list))


(define (string-replacement-length-change replacement)
  (- (string-replacement-new-span replacement) (string-replacement-span replacement)))


(define (string-replacement-range replacement)
  (closed-open-range
   (string-replacement-start replacement)
   (string-replacement-end replacement)
   #:comparator natural<=>))


(define (string-replacement-overlaps? replacement other-replacement)
  (range-overlaps?
   (string-replacement-range replacement) (string-replacement-range other-replacement)))


(define-tuple-type inserted-string (contents))
(define-tuple-type copied-string (start end))


(define (replacement-string-span piece)
  (match piece
    [(inserted-string inserted-string) (string-length inserted-string)]
    [(copied-string start end) (- end start)]))


(define (string-replacement-render replacement original-string)
  (define required-length (string-replacement-required-length replacement))
  (define original-length (string-length original-string))
  (unless (>= original-length required-length)
    (raise-arguments-error
     (name string-apply-replacement)
     "string is not long enough"
     "string" original-string
     "string length" original-length
     "required length" required-length))
  (define content-length (string-replacement-new-span replacement))
  (define edited (make-string content-length))
  (for/fold ([start 0] #:result (void))
            ([piece (in-list (string-replacement-contents replacement))])
    (match piece
      [(inserted-string inserted)
       (string-copy! edited start inserted)
       (+ start (string-length inserted))]
      [(copied-string copy-start copy-end)
       (string-copy! edited start original-string copy-start copy-end)
       (+ start (- copy-end copy-start))]))
  (string->immutable-string edited))


(define (string-apply-replacement string replacement)
  (define start (string-replacement-start replacement))
  (define end (string-replacement-end replacement))
  (define new-end (string-replacement-new-end replacement))
  (define contents (string-replacement-contents replacement))
  (define required-length (string-replacement-required-length replacement))
  (define original-length (string-length string))
  (unless (>= original-length required-length)
    (raise-arguments-error
     (name string-apply-replacement)
     "string is not long enough"
     "string" string
     "string length" original-length
     "required length" required-length))
  (define new-length (+ original-length (string-replacement-length-change replacement)))
  (define edited (make-string new-length))
  (string-copy! edited 0 string 0 start)
  (for/fold ([start start] #:result (void))
            ([piece (in-list contents)])
    (match piece
      [(inserted-string inserted)
       (string-copy! edited start inserted)
       (+ start (string-length inserted))]
      [(copied-string copy-start copy-end)
       (string-copy! edited start string copy-start copy-end)
       (+ start (- copy-end copy-start))]))
  (string-copy! edited new-end string end)
  (string->immutable-string edited))


(module+ test
  (test-case (name-string string-apply-replacement)
    (define s "good morning and hello world")
    (define replacement-pieces
      (list
       (inserted-string "evening")
       (copied-string 12 17)
       (inserted-string "goodbye")))
    (define replacement
      (string-replacement
       #:start 5
       #:end 22
       #:contents replacement-pieces))
    (check-equal? (string-replacement-span replacement) 17)
    (check-equal? (string-replacement-new-span replacement) 19)
    (check-equal? (string-replacement-length-change replacement) 2)
    (check-equal? (string-replacement-new-end replacement) 24)
    (check-equal? (string-replacement-render replacement s) "evening and goodbye")
    (check-equal? (string-apply-replacement s replacement) "good evening and goodbye world")))


(define (file-apply-string-replacement! path replacement)
  (define replacement-text (string-apply-replacement (file->string path #:mode 'text) replacement))
  (display-to-file replacement-text path #:mode 'text #:exists 'replace))


(define/guard (string-replacement-union replacement1 replacement2)
  (guard (<= (string-replacement-start replacement1) (string-replacement-start replacement2)) else
    (string-replacement-union replacement2 replacement1))
  (define piece-between
    (copied-string (string-replacement-end replacement1) (string-replacement-start replacement2)))
  (string-replacement
   #:start (string-replacement-start replacement1)
   #:end (string-replacement-end replacement2)
   #:contents
   (append
    (string-replacement-contents replacement1)
    (list piece-between)
    (string-replacement-contents replacement2))))


(define union-into-string-replacement
  (make-fold-reducer
   (λ (accumulated replacement)
     (cond
       [(string-replacement-overlaps? accumulated replacement)
        (printf "overlapped at position ~a, rejecting\n" (string-replacement-start replacement))
        accumulated]
       [else (string-replacement-union accumulated replacement)]))
   (string-replacement #:start 0 #:end 0 #:contents (list))
   #:name (name union-into-string-replacement)))


(module+ test
  (test-case (name-string string-replacement-union)
    (define s "hello world")
    (define r1 (string-replacement #:start 0 #:end 5 #:contents (list (inserted-string "goodbye"))))
    (define r2 (string-replacement #:start 6 #:end 11 #:contents (list (inserted-string "friend"))))
    (check-false (string-replacement-overlaps? r1 r2))
    (define union (string-replacement-union r1 r2))
    (define expected
      (string-replacement
       #:start 0
       #:end 11
       #:contents (list (inserted-string "goodbye") (copied-string 5 6) (inserted-string "friend"))))
    (check-equal? union expected)
    (check-equal? (transduce (list r1 r2) #:into union-into-string-replacement) expected)))
