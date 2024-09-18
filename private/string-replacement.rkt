#lang racket/base

;; A string replacement is an operation that can be applied to a string which can insert, delete, and
;; move around the contents of the string.


(require racket/contract/base)


(provide
 (contract-out
  [string-replacement? (-> any/c boolean?)]
  [string-replacement
   (->i
    #:chaperone
    (#:start [start natural?]
     #:end [end natural?]
     #:contents [contents (sequence/c (or/c inserted-string? copied-string?))])
    #:pre/name (start end) "end cannot be before start" (<= start end)
    [_ string-replacement?])]
  [replacement-string-span (-> (or/c inserted-string? copied-string?) exact-nonnegative-integer?)]
  [string-replacement-start (-> string-replacement? natural?)]
  [string-replacement-original-end (-> string-replacement? natural?)]
  [string-replacement-original-span (-> string-replacement? natural?)]
  [string-replacement-new-end (-> string-replacement? natural?)]
  [string-replacement-new-span (-> string-replacement? natural?)]
  [string-replacement-contents
   (-> string-replacement? (listof (or/c inserted-string? copied-string?)))]
  [string-replacement-preserved-locations (-> string-replacement? range-set?)]
  [string-replacement-overlaps? (-> string-replacement? string-replacement? boolean?)]
  [string-replacement-normalize
   (->* (string-replacement? string?)
        (#:preserve-start (or/c exact-nonnegative-integer? #false)
         #:preserve-end (or/c exact-nonnegative-integer? #false))
        string-replacement?)]
  [string-replacement-union
   (->i ([replacement1 string-replacement?]
         [replacement2 string-replacement?])
        #:pre/name (replacement1 replacement2)
        "replacements must not overlap"
        (not (string-replacement-overlaps? replacement1 replacement2))
        [_ string-replacement?])]
  [union-into-string-replacement (reducer/c string-replacement? string-replacement?)]
  [string-replacement-render (-> string-replacement? string? immutable-string?)]
  [string-apply-replacement (-> string? string-replacement? immutable-string?)]
  [file-apply-string-replacement! (-> path-string? string-replacement? void?)]
  [inserted-string? (-> any/c boolean?)]
  [inserted-string (-> string? inserted-string?)]
  [inserted-string-contents (-> inserted-string? immutable-string?)]
  [copied-string? (-> any/c boolean?)]
  [copied-string
   (->i
    #:chaperone
    ([start natural?]
     [end natural?])
    #:pre/name (start end) "end cannot be before start" (<= start end)
    [_ copied-string?])]
  [copied-string-start (-> copied-string? natural?)]
  [copied-string-end (-> copied-string? natural?)]))


(require guard
         racket/file
         racket/match
         racket/math
         racket/sequence
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/range
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/private/static-name
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple
         resyntax/private/source)


(module+ test
  (require rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-record-type string-replacement
  (start original-end original-span new-end new-span required-length contents)
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
   #:original-end end
   #:original-span (- end start)
   #:new-end (+ start new-span)
   #:new-span new-span
   #:required-length (option-get max-end end)
   #:contents content-list))


(define (string-replacement-length-change replacement)
  (- (string-replacement-new-span replacement) (string-replacement-original-span replacement)))


(define (string-replacement-range replacement)
  (closed-open-range
   (string-replacement-start replacement)
   (string-replacement-original-end replacement)
   #:comparator natural<=>))


(define (string-replacement-overlaps? replacement other-replacement)
  (range-overlaps?
   (string-replacement-range replacement) (string-replacement-range other-replacement)))


(struct inserted-string (contents) #:transparent
  #:guard (λ (contents _) (string->immutable-string contents))
  #:property prop:custom-print-quotable 'never)


(define-tuple-type copied-string (start end))


(define (replacement-string-span piece)
  (match piece
    [(inserted-string inserted-string) (string-length inserted-string)]
    [(copied-string start end) (- end start)]))


(define (replacement-string-drop-left piece amount)
  (match piece
    [(inserted-string s)
     (inserted-string (string->immutable-string (substring s amount)))]
    [(copied-string start end) (copied-string (+ start amount) end)]))


(define (replacement-string-drop-right piece amount)
  (match piece
    [(inserted-string s)
     (define new-length (- (string-length s) amount))
     (inserted-string (string->immutable-string (substring s 0 new-length)))]
    [(copied-string start end) (copied-string start (- end amount))]))


(define (string-replacement-render replacement original-string)
  (define immutable-original-string (string->immutable-string original-string))
  (define required-length (string-replacement-required-length replacement))
  (define original-length (string-length immutable-original-string))
  (unless (>= original-length required-length)
    (raise-arguments-error
     (name string-apply-replacement)
     "string is not long enough"
     "string" immutable-original-string
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
       (string-copy! edited start immutable-original-string copy-start copy-end)
       (+ start (- copy-end copy-start))]))
  (string->immutable-string edited))


(define (string-apply-replacement string replacement)
  (define immutable-string (string->immutable-string string))
  (define start (string-replacement-start replacement))
  (define end (string-replacement-original-end replacement))
  (define new-end (string-replacement-new-end replacement))
  (define contents (string-replacement-contents replacement))
  (define required-length (string-replacement-required-length replacement))
  (define original-length (string-length immutable-string))
  (unless (>= original-length required-length)
    (raise-arguments-error
     (name string-apply-replacement)
     "string is not long enough"
     "string" immutable-string
     "string length" original-length
     "required length" required-length))
  (define new-length (+ original-length (string-replacement-length-change replacement)))
  (define edited (make-string new-length))
  (string-copy! edited 0 immutable-string 0 start)
  (for/fold ([start start] #:result (void))
            ([piece (in-list contents)])
    (match piece
      [(inserted-string inserted)
       (string-copy! edited start inserted)
       (+ start (string-length inserted))]
      [(copied-string copy-start copy-end)
       (string-copy! edited start immutable-string copy-start copy-end)
       (+ start (- copy-end copy-start))]))
  (string-copy! edited new-end immutable-string end)
  (string->immutable-string edited))


(module+ test
  (test-case (name-string string-apply-replacement)

    (test-case "replace middle part"
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
      (check-equal? (string-replacement-original-span replacement) 17)
      (check-equal? (string-replacement-new-span replacement) 19)
      (check-equal? (string-replacement-length-change replacement) 2)
      (check-equal? (string-replacement-new-end replacement) 24)
      (check-equal? (string-replacement-render replacement s) "evening and goodbye")
      (check-equal? (string-apply-replacement s replacement) "good evening and goodbye world"))

    (test-case "replace entire string"
      (define s "good morning and hello world")
      (define replacement
        (string-replacement #:start 0
                            #:end (string-length s)
                            #:contents (list (inserted-string "hi there"))))
      (check-equal? (string-apply-replacement s replacement) "hi there"))))


(define (file-apply-string-replacement! path replacement)
  (define replacement-text (string-apply-replacement (source->string (file-source path)) replacement))
  (display-to-file replacement-text path #:mode 'text #:exists 'replace))


(define/guard (string-replacement-normalize replacement original-string
                                            #:preserve-start [preserve-start #false]
                                            #:preserve-end [preserve-end #false])
  (define replaced (string-apply-replacement original-string replacement))
  (guard (not (equal? original-string replaced)) #:else replacement)
  (define actual-start
    (inexact->exact (min (or preserve-start +inf.0) (string-diff-start original-string replaced))))
  (define actual-end
    (inexact->exact
     (max actual-start (or preserve-end -inf.0) (string-diff-end original-string replaced))))
  (define left-trimmed-pieces
    (let loop ([pieces (string-replacement-contents replacement)]
               [pos (string-replacement-start replacement)])
      (guarded-block
        (guard (< pos actual-start) #:else pieces)
        (guard-match (cons next-piece remaining) pieces #:else (list))
        (define piece-span (replacement-string-span next-piece))
        (if (< (+ pos piece-span) actual-start)
            (loop remaining (+ pos piece-span))
            (cons (replacement-string-drop-left next-piece (- actual-start pos)) remaining)))))
  (define right-trimmed-pieces
    (let loop ([pieces (reverse left-trimmed-pieces)]
               [pos (string-replacement-original-end replacement)])
      (guarded-block
        (guard (> pos actual-end) #:else (reverse pieces))
        (guard-match (cons next-piece remaining) pieces #:else (list))
        (define piece-span (replacement-string-span next-piece))
        (if (> (- pos piece-span) actual-start)
            (loop remaining (- pos piece-span))
            (reverse
             (cons (replacement-string-drop-right next-piece (- pos actual-start)) remaining))))))
  (string-replacement #:start actual-start
                      #:end actual-end
                      #:contents right-trimmed-pieces))


(define (string-diff-start original new)
  (let loop ([i 0])
    (cond
      [(or (>= i (string-length original)) (>= i (string-length new))) #false]
      [(equal? (string-ref original i) (string-ref new i)) (loop (add1 i))]
      [else i])))


(define (string-diff-end original new)
  (let loop ([i 0])
    (guarded-block
      (guard (and (< i (string-length original)) (< i (string-length new))) #:else #false)
      (define orig-index (- (string-length original) i 1))
      (define new-index (- (string-length new) i 1))
      (if (equal? (string-ref original orig-index) (string-ref new new-index))
          (loop (add1 i))
          (+ orig-index 1)))))


(module+ test

  (test-case "string-diff-start"
    (check-false (string-diff-start "aaaaaa" "aaaaaa"))
    (check-equal? (string-diff-start "aaaaaa" "baaaaa") 0)
    (check-equal? (string-diff-start "aaaaaa" "abaaaa") 1)
    (check-equal? (string-diff-start "aaaaaa" "aabaaa") 2)
    (check-equal? (string-diff-start "aaaaaa" "aaabaa") 3)
    (check-equal? (string-diff-start "aaaaaa" "aaaaba") 4)
    (check-equal? (string-diff-start "aaaaaa" "aaaaab") 5)
    (check-equal? (string-diff-start "aaaaaa" "b") 0)
    (check-equal? (string-diff-start "aaaaaa" "ab") 1)
    (check-equal? (string-diff-start "aaaaaa" "aab") 2)
    (check-equal? (string-diff-start "aaaaaa" "aaab") 3)
    (check-equal? (string-diff-start "aaaaaa" "aaaab") 4)
    (check-equal? (string-diff-start "aaaa" "abaaa") 1)
    (check-equal? (string-diff-start "aaaa" "aaaba") 3)
    (check-equal? (string-diff-start "good morning and hello world"
                                     "good morning friend and hello world")
                  13))

  (test-case "string-diff-end"
    (check-false (string-diff-end "aaaaaa" "aaaaaa"))
    (check-equal? (string-diff-end "aaaaaa" "baaaaa") 1)
    (check-equal? (string-diff-end "aaaaaa" "abaaaa") 2)
    (check-equal? (string-diff-end "aaaaaa" "aabaaa") 3)
    (check-equal? (string-diff-end "aaaaaa" "aaabaa") 4)
    (check-equal? (string-diff-end "aaaaaa" "aaaaba") 5)
    (check-equal? (string-diff-end "aaaaaa" "aaaaab") 6)
    (check-equal? (string-diff-end "aaaaaa" "baaaa") 2)
    (check-equal? (string-diff-end "aaaaaa" "baaa") 3)
    (check-equal? (string-diff-end "aaaaaa" "baa") 4)
    (check-equal? (string-diff-end "aaaaaa" "ba") 5)
    (check-equal? (string-diff-end "aaaaaa" "b") 6)
    (check-equal? (string-diff-end "aaaa" "abaaa") 1)
    (check-equal? (string-diff-end "aaaa" "aaaba") 3)
    (check-equal? (string-diff-end "good morning and hello world"
                                   "good morning friend and hello world")
                  12))

  (test-case "string-replacement-normalize"

    (test-case "empty replacement"
      (define s "good morning and hello world")
      (define replacement-pieces (list))
      (define replacement
        (string-replacement
         #:start 5
         #:end 5
         #:contents replacement-pieces))
      (check-equal? (string-replacement-normalize replacement s) replacement))

    (test-case "insertion-only replacement"
      (define s "good morning and hello world")
      (define replacement-pieces (list (inserted-string "friend ")))
      (define replacement
        (string-replacement
         #:start 13
         #:end 13
         #:contents replacement-pieces))
      (check-equal? (string-replacement-normalize replacement s) replacement))

    (test-case "redundant copied string before insertion"
      (define s "good morning and hello world")
      (define replacement-pieces (list (copied-string 5 13) (inserted-string "friend ")))
      (define replacement
        (string-replacement
         #:start 5
         #:end 13
         #:contents replacement-pieces))
      (check-equal? (string-replacement-normalize replacement s)
                    (string-replacement #:start 13
                                        #:end 13
                                        #:contents (list (inserted-string "friend ")))))

    (test-case "redundant inserted string before insertion"
      (define s "good morning and hello world")
      (define replacement-pieces (list (inserted-string "morning ") (inserted-string "friend ")))
      (define replacement
        (string-replacement
         #:start 5
         #:end 13
         #:contents replacement-pieces))
      (check-equal? (string-replacement-normalize replacement s)
                    (string-replacement #:start 13
                                        #:end 13
                                        #:contents (list (inserted-string "friend ")))))

    (test-case "redundant copied string after insertion"
      (define s "good morning and hello world")
      (define replacement-pieces (list (inserted-string "friend ") (copied-string 13 16)))
      (define replacement
        (string-replacement
         #:start 13
         #:end 16
         #:contents replacement-pieces))
      (check-equal? (string-replacement-normalize replacement s)
                    (string-replacement #:start 13
                                        #:end 13
                                        #:contents (list (inserted-string "friend ")))))

    (test-case "redundant inserted string after insertion"
      (define s "good morning and hello world")
      (define replacement-pieces (list (inserted-string "friend ") (inserted-string "and")))
      (define replacement
        (string-replacement
         #:start 13
         #:end 16
         #:contents replacement-pieces))
      (check-equal? (string-replacement-normalize replacement s)
                    (string-replacement #:start 13
                                        #:end 13
                                        #:contents (list (inserted-string "friend ")))))))


(define/guard (string-replacement-union replacement1 replacement2)
  (guard (<= (string-replacement-start replacement1) (string-replacement-start replacement2)) #:else
    (string-replacement-union replacement2 replacement1))
  (define piece-between
    (copied-string
     (string-replacement-original-end replacement1) (string-replacement-start replacement2)))
  (string-replacement
   #:start (string-replacement-start replacement1)
   #:end (string-replacement-original-end replacement2)
   #:contents
   (append
    (string-replacement-contents replacement1)
    (list piece-between)
    (string-replacement-contents replacement2))))


(define union-into-string-replacement
  (make-fold-reducer
   string-replacement-union
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


(define (string-replacement-preserved-locations replacement)
  (define before-replacement
    (less-than-range (string-replacement-start replacement) #:comparator natural<=>))
  (define after-replacement
    (at-least-range (string-replacement-original-end replacement) #:comparator natural<=>))
  (for/fold ([ranges (range-set before-replacement after-replacement)])
            ([piece (in-list (string-replacement-contents replacement))]
             #:when (copied-string? piece))
    (match-define (copied-string start end) piece)
    (range-set-add ranges (closed-open-range start end #:comparator natural<=>))))
