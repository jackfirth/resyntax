#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [line-replacement
   (-> #:start-line exact-positive-integer?
       #:original-lines (sequence/c string?)
       #:new-lines (sequence/c string?)
       line-replacement?)]
  [line-replacement? predicate/c]
  [line-replacement-start-line (-> line-replacement? exact-positive-integer?)]
  [line-replacement-original-end-line (-> line-replacement? exact-positive-integer?)]
  [line-replacement-original-lines
   (-> line-replacement? (vectorof (and/c string? immutable?) #:immutable #true #:flat? #true))]
  [line-replacement-new-end-line (-> line-replacement? exact-positive-integer?)]
  [line-replacement-new-lines
   (-> line-replacement? (vectorof (and/c string? immutable?) #:immutable #true #:flat? #true))]
  [line-replacement-new-text (-> line-replacement? (and/c string? immutable?))]
  [string-replacement->line-replacement (-> string-replacement? string? line-replacement?)]))


(require racket/sequence
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/linemap
         resyntax/private/string-replacement)


(module+ test
  (require (submod "..")
           rackunit))


;@----------------------------------------------------------------------------------------------------


(define-record-type line-replacement (start-line original-lines new-lines)
  #:omit-root-binding)


(define (line-replacement #:start-line start-line
                          #:original-lines original-lines
                          #:new-lines new-lines)
  (define immutable-original-lines
    (for/vector ([line original-lines])
      (string->immutable-string line)))
  (define immutable-new-lines
    (for/vector ([line new-lines])
      (string->immutable-string line)))
  (constructor:line-replacement #:start-line start-line
                                #:original-lines immutable-original-lines
                                #:new-lines immutable-new-lines))


(define (line-replacement-original-end-line replacement)
  (+ (line-replacement-start-line replacement)
     (sub1 (vector-length (line-replacement-original-lines replacement)))))


(define (line-replacement-new-end-line replacement)
  (+ (line-replacement-start-line replacement)
     (sub1 (vector-length (line-replacement-new-lines replacement)))))


(define (line-replacement-new-text replacement)
  (transduce (line-replacement-new-lines replacement) #:into (join-into-string "\n")))


(define (string-replacement->line-replacement replacement original-string)
  (define new-string (string-apply-replacement original-string replacement))
  (define orig-lmap (string-linemap original-string))
  (define new-lmap (string-linemap new-string))
  
  (define start-line
    (linemap-position-to-line orig-lmap (add1 (string-replacement-start replacement))))
  (define start-pos
    (sub1
     (linemap-position-to-start-of-line orig-lmap (add1 (string-replacement-start replacement)))))
  (define original-end-pos
    (sub1
     (linemap-position-to-end-of-line orig-lmap
                                      (add1 (string-replacement-original-end replacement)))))
  (define new-end-pos
    (sub1 (linemap-position-to-end-of-line new-lmap (add1 (string-replacement-new-end replacement)))))

  (define original-substr (substring original-string start-pos original-end-pos))
  (define new-substr (substring new-string start-pos new-end-pos))
  (line-replacement #:start-line start-line
                    #:original-lines (in-lines (open-input-string original-substr))
                    #:new-lines (in-lines (open-input-string new-substr))))


(module+ test
  (test-case "string-replacement->line-replacement"

    (test-case "multiple middle lines"
      (define s "hello\nworld\nhow are you\ntoday?")
      (define middle-of-world-index 8)
      (define start-of-are-you-index 16)
      (check-equal? (substring s middle-of-world-index start-of-are-you-index) "rld\nhow ")
      (define str-replacement
        (string-replacement #:start middle-of-world-index
                            #:end start-of-are-you-index
                            #:contents (list (inserted-string "RLD HOW "))))
      (check-equal? (string-replacement->line-replacement str-replacement s)
                    (line-replacement #:start-line 2
                                      #:original-lines (list "world" "how are you")
                                      #:new-lines (list "woRLD HOW are you"))))

    (test-case "entire string replacement"
      (define s "hello\nworld\nhow are you\ntoday?")
      (define str-replacement
        (string-replacement #:start 0
                            #:end (string-length s)
                            #:contents (list (inserted-string "hello world"))))
      (check-equal? (string-replacement->line-replacement str-replacement s)
                    (line-replacement #:start-line 1
                                      #:original-lines (list "hello" "world" "how are you" "today?")
                                      #:new-lines (list "hello world"))))))
