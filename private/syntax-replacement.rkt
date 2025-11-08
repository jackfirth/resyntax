#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [syntax-replacement? (-> any/c boolean?)]
  [syntax-replacement
   (-> #:original-syntax (and/c syntax? syntax-original? syntax-has-original-path?)
       #:new-syntax syntax?
       #:source source?
       #:introduction-scope (->* (syntax?) ((or/c 'flip 'add 'remove)) syntax?)
       #:uses-universal-tagged-syntax? boolean?
       syntax-replacement?)]
  [syntax-replacement-render (-> syntax-replacement? string-replacement?)]
  [syntax-replacement-original-syntax
   (-> syntax-replacement? (and/c syntax? syntax-has-original-path?))]
  [syntax-replacement-new-syntax (-> syntax-replacement? syntax?)]
  [syntax-replacement-source (-> syntax-replacement? source?)]
  [syntax-replacement-introduction-scope
   (-> syntax-replacement? (->* (syntax?) ((or/c 'flip 'add 'remove)) syntax?))]
  [syntax-replacement-introduces-incorrect-bindings? (-> syntax-replacement? boolean?)]
  [syntax-replacement-introduced-incorrect-identifiers
   (-> syntax-replacement? (listof identifier?))]
  [syntax-replacement-preserves-comments? (-> syntax-replacement? range-set? boolean?)]
  [syntax-replacement-dropped-comment-locations (-> syntax-replacement? range-set? range-set?)]))


(require (for-syntax racket/base)
         fmt
         guard
         racket/format
         racket/list
         racket/match
         racket/pretty
         racket/sequence
         racket/stream
         racket/string
         rebellion/base/comparator
         rebellion/collection/range-set
         rebellion/private/static-name
         rebellion/type/record
         resyntax/private/linemap
         resyntax/private/logger
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/string-replacement
         resyntax/private/syntax-neighbors
         (only-in resyntax/private/syntax-traversal syntax-search-everything)
         syntax/parse
         syntax/parse/experimental/template
         (only-in rebellion/base/range closed-open-range)
         (only-in resyntax/default-recommendations/private/syntax-identifier-sets
                  in-syntax-identifiers))


(module+ test
  (require fmt
           racket/port
           racket/string
           rackunit
           (submod "..")))


;@----------------------------------------------------------------------------------------------------


(define-record-type syntax-replacement
  (original-syntax new-syntax source introduction-scope uses-universal-tagged-syntax?))


(struct focus (contents) #:transparent)


(define (syntax-replacement-render replacement #:format? [format? #true])
  (if (syntax-replacement-uses-universal-tagged-syntax? replacement)
      (syntax-replacement-render-using-uts replacement)
      (syntax-replacement-render-using-s-expressions replacement #:format? format?)))


(define (syntax-replacement-render-using-uts replacement)

  (define/guard (pieces stx)
    (guard (not (syntax-completely-original? stx)) #:else
      (log-resyntax-debug "copying original syntax ~a" stx)
      (define start (sub1 (syntax-position stx)))
      (define end (+ start (syntax-span stx)))
      (list (copied-string start end)))
    (syntax-parse stx
      [(tag:keyword subform ...)
       (define separators (syntax-property (attribute tag) 'uts-separators))
       (unless separators
         (raise-arguments-error
          'syntax-replacement-render-using-uts
          (string-append "cannot render as universal taggged syntax, tag node does not contain"
                         " 'uts-separators syntax property")
          "subform" this-syntax
          "tag" (attribute tag)
          "replacement" replacement))
       (define child-piece-lists
         (map pieces (attribute subform)))
       (for/fold ([piece-lists (list (list (inserted-string (first separators))))]
                  #:result (append* (reverse piece-lists)))
                 ([child-list (in-list child-piece-lists)]
                  [separator-after (in-list (rest separators))])
         (list* (list (inserted-string separator-after)) child-list piece-lists))]
      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
       (define content (syntax-property (attribute v) 'uts-content))
       (unless content
         (raise-arguments-error
          'syntax-replacement-render-using-uts
          (string-append "cannot render as universal tagged syntax, atom does not contain"
                         " 'uts-content syntax property")
          "atom" this-syntax
          "replacement" replacement))
       (list (inserted-string content))]))

  (match-define (syntax-replacement #:original-syntax orig-stx #:new-syntax new-stx #:source source)
    replacement)
  (define start (sub1 (syntax-position orig-stx)))
  (define end (+ start (syntax-span orig-stx)))
  (define contents (pieces new-stx))
  (define rendered (string-replacement #:start start #:end end #:contents contents))
  (when (log-level? resyntax-logger 'debug)
    (define message (string-indent (pretty-format rendered) #:amount 2))
    (log-resyntax-debug "string replacement contents:\n~a" message))
  rendered)


(define (syntax-replacement-render-using-s-expressions replacement #:format? [format? #true])

  (define/guard (pieces stx #:focused? [focused? #false])
    (guard (or focused? (not (syntax-property stx 'focus-replacement-on))) #:else
      (log-resyntax-debug "focusing in on ~a" stx)
      (list (focus (pieces stx #:focused? #true))))
    (guard (not (syntax-completely-original? stx)) #:else
      (log-resyntax-debug "copying original syntax ~a" stx)
      (define start (sub1 (syntax-position stx)))
      (define end (+ start (syntax-span stx)))
      (list (copied-string start end)))
    (syntax-parse stx
      #:literals (quote)

      [(~or v:id v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
       (list (inserted-string (string->immutable-string (~s (syntax-e #'v)))))]
      
      [(quote datum) (cons (inserted-string "'") (pieces #'datum #:focused? focused?))]
      
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
               (append (pieces subform-stx #:focused? focused?) (list separator-piece))
               (pieces subform-stx #:focused? focused?))))
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
               (append (pieces subform-stx #:focused? focused?) (list separator-piece))
               (pieces subform-stx #:focused? focused?))))
       (define tail-pieces (pieces #'tail-form #:focused? focused?))
       (define dot-piece
         (or (original-separator-piece (last (attribute subform)) #'tail-form)
             (inserted-string " . ")))
       (append
        (list (inserted-string opener))
        (apply append subform-piece-lists)
        (list dot-piece)
        tail-pieces
        (list (inserted-string closer)))]))

  (match-define (syntax-replacement #:original-syntax orig-stx #:new-syntax new-stx #:source source)
    replacement)
  (define start (sub1 (syntax-position orig-stx)))
  (define end (+ start (syntax-span orig-stx)))
  (define contents-with-possible-focus (pieces new-stx))
  (define has-focus? (and (findf focus? contents-with-possible-focus) #true))
  (define focused-start
    (+ start
       (for/sum ([piece (in-list contents-with-possible-focus)]
                 #:break (focus? piece))
         (replacement-string-span piece))))
  (define focused-end
    (- end
       (for/sum ([piece (in-list (reverse contents-with-possible-focus))]
                 #:break (focus? piece))
         (replacement-string-span piece))))
  (define raw-contents
    (append-map (λ (piece) (if (focus? piece) (focus-contents piece) (list piece)))
                contents-with-possible-focus))
  (define unformatted
    (string-replacement #:start start
                        #:end end
                        #:contents raw-contents))
  (when (log-level? resyntax-logger 'debug)
    (define message
      (string-indent (pretty-format unformatted) #:amount 2))
    (log-resyntax-debug "string replacement contents:\n~a" message))
  (cond
    [(not format?) unformatted]
    [else
     (define normalized
       (if has-focus?
           (string-replacement-normalize unformatted (source->string source)
                                         #:preserve-start focused-start
                                         #:preserve-end focused-end)
           unformatted))
     (when has-focus?
       (log-resyntax-debug "string replacement after focusing:\n~a"
                           (string-indent (pretty-format normalized) #:amount 2)))
     (string-replacement-format normalized (source->string source))]))


(define/guard (original-separator-piece stx trailing-stx)
  (guard (syntax-originally-neighbors? stx trailing-stx) #:else #false)
  (let-values ([(stx trailing-stx) (syntax-extract-originals-from-pair stx trailing-stx)])
    (log-resyntax-debug "copying separator between ~a and ~a" stx trailing-stx)
    (define stx-end (+ (sub1 (syntax-position stx)) (syntax-span stx)))
    (define trailing-start (sub1 (syntax-position trailing-stx)))
    (copied-string stx-end trailing-start)))


;; This function is the sole integration point between Resyntax and fmt. It is used by Resyntax to
;; format any refactored code initially generated by Resyntax.
(define (string-replacement-format replacement original)
  (define refactored-source-code (string-apply-replacement original replacement))
  (define start (string-replacement-start replacement))
  (define end (string-replacement-new-end replacement))
  (define changed-code-substring (substring refactored-source-code start end))
  (define initial-columns (string-column-offset refactored-source-code start))
  (log-resyntax-debug "unformatted code after applying replacement:\n~a"
                      (string-indent refactored-source-code #:amount 2))
  (log-resyntax-debug "about to format unformatted code at indentation ~a:\n~a"
                      initial-columns (string-indent changed-code-substring #:amount 2))

  ;; We could use the #:indent argument to program-format instead of lying to it about how much
  ;; horizontal space is available and indenting the resulting string. However, fmt has some odd
  ;; behavior in how it handles formatting regions with multiple expressions (sorawee/fmt#70) and
  ;; regions with commented top-level expressions (sorawee/fmt#68).
  (define base-allowed-width (- (current-width) initial-columns))
  
  ;; For single-line replacements, we need to account for trailing text after the replacement
  ;; to ensure the formatted code doesn't exceed the line length limit.
  (define trailing-text-length
    (let ([linemap (string-linemap original)]
          [orig-end (string-replacement-original-end replacement)])
      ;; Convert from 0-based string indices to 1-based positions for linemap
      (if (= (linemap-position-to-line linemap (add1 start))
             (linemap-position-to-line linemap (add1 orig-end)))
          (- (linemap-position-to-end-of-line linemap (add1 orig-end)) (add1 orig-end))
          0)))
  
  (define allowed-width (- base-allowed-width trailing-text-length))
  (define formatted-code-substring
    (string-hanging-indent (program-format changed-code-substring #:width allowed-width)
                           #:amount initial-columns))

  (string-replacement #:start start
                      #:end (string-replacement-original-end replacement)
                      #:contents (list (inserted-string formatted-code-substring))))


(define/guard (string-column-offset str position)
  (define up-to-position (substring str 0 position))
  (guard (non-empty-string? up-to-position) #:else 0)
  (string-length (last (string-split up-to-position "\n" #:trim? #false))))


(module+ test
  (test-case "string-column-offset"
    (check-equal? (string-column-offset "" 0) 0)
    (check-equal? (string-column-offset "apple" 0) 0)
    (check-equal? (string-column-offset "apple" 4) 4)
    (check-equal? (string-column-offset "apple\nbanana" 0) 0)
    (check-equal? (string-column-offset "apple\nbanana" 4) 4)
    (check-equal? (string-column-offset "apple\nbanana" 6) 0)
    (check-equal? (string-column-offset "apple\nbanana" 8) 2)
    (check-equal? (string-column-offset "apple\nbanana" 12) 6)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 0) 0)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 4) 4)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 6) 0)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 8) 2)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 12) 6)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 13) 0)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 16) 3)
    (check-equal? (string-column-offset "apple\nbanana\ngrape" 18) 5)))


(define/guard (shift-left vs)
  (guard-match (cons _ shifted-vs) vs #:else '())
  (append shifted-vs (list #false)))


(module+ test
  (test-case (name-string syntax-replacement-render)
    (define orig-code "(+ 1 (+ 2 3))")
    (define orig-stx (syntax-label-original-paths (with-input-from-string orig-code read-syntax)))
    (define orig-start (sub1 (syntax-position orig-stx)))
    (define flip (make-syntax-introducer))
    (define new-stx
      (flip
       (syntax-parse (flip orig-stx)
         #:datum-literals (+)
         [((~and + +_1) x (+ y z)) #'(+_1 x y z)])))
    (define replacement
      (syntax-replacement #:original-syntax orig-stx
                          #:new-syntax new-stx
                          #:source (string-source orig-code)
                          #:introduction-scope flip
                          #:uses-universal-tagged-syntax? #false))
    (define expected
      (string-replacement #:start 0 #:end 13 #:contents (list (inserted-string "(+ 1 2 3)"))))
    (check-equal? (syntax-replacement-render replacement) expected))

  (test-case "string-replacement-format considers trailing text on single-line replacements"
    ;; Test that when formatting a single-line replacement, we account for trailing text
    ;; to avoid exceeding the line length limit. This test simulates the issue from
    ;; herbie-fp/herbie#1391 where a quasiquote is replaced with a list call.
    (define orig-code "      [x `(a b c)]")
    (define replacement
      (string-replacement #:start 9
                          #:end 18
                          #:contents (list (inserted-string "(list a b c)"))))
    ;; The original code is 18 characters. With trailing "]", it's 19 characters total.
    ;; If we format without accounting for the trailing "]", the formatted result might
    ;; exceed the desired line length.
    (parameterize ([current-width 102])
      (define formatted (string-replacement-format replacement orig-code))
      (define result (string-apply-replacement orig-code formatted))
      ;; The result should fit within a reasonable line length and shouldn't be multiline
      (check-false (string-contains? result "\n")
                   "Result should remain on a single line")
      ;; Verify the replacement happened
      (check-true (string-contains? result "(list a b c)")
                  "Result should contain the list form")))

  (test-case "realistic example from herbie-fp/herbie#1391"
    ;; This test simulates the actual issue where a 102-character line would become 103 characters
    ;; Without the fix, the formatter would allow the code to exceed the line length limit.
    ;; With the fix, the formatter either keeps it on one line within the limit, or breaks it
    ;; across multiple lines if it cannot fit.
    (define orig-line
      (string-append "      [`(,(and (or '+ '- '* '/ 'and 'or) op) ,as ..2 ,b)"
                     " `(,op ,(loop `(,op ,@as) env) ,(loop b env))]"))
    (check-equal? (string-length orig-line) 102 "Original line should be 102 characters")
    
    ;; The quasiquote expression starts at position 57 and ends at position 101
    ;; Original: `(,op ,(loop `(,op ,@as) env) ,(loop b env))
    ;; Replacement: (list op (loop `(,op ,@as) env) (loop b env))
    (define quasiquote-start 57)
    (define quasiquote-end 101) ; just before the final ]
    
    (define replacement
      (string-replacement
       #:start quasiquote-start
       #:end quasiquote-end
       #:contents (list (inserted-string "(list op (loop `(,op ,@as) env) (loop b env))"))))
    
    ;; Test with Racket's standard line width of 102
    (parameterize ([current-width 102])
      (define formatted (string-replacement-format replacement orig-line))
      (define result (string-apply-replacement orig-line formatted))
      
      ;; If the result is a single line, it should not exceed 102 characters
      ;; If it's multi-line, each line should not exceed 102 characters
      (for ([line (in-list (string-split result "\n"))])
        (check-true (<= (string-length line) 102)
                    (format "Line exceeds length limit: ~a chars (should be <= 102)"
                            (string-length line)))))))


(define (syntax-replacement-introduces-incorrect-bindings? replacement)
  (match-define (syntax-replacement #:original-syntax orig
                                    #:new-syntax new
                                    #:introduction-scope intro)
    replacement)
  (for/and ([new-id (in-syntax-identifiers new)]
            #:unless (bound-identifier=? new-id (intro new-id 'remove))
            #:unless (syntax-property new-id 'skip-incorrect-binding-check?))
    (free-identifier=? new-id (datum->syntax orig (syntax->datum new-id)))))


(define (syntax-replacement-introduced-incorrect-identifiers replacement)
  (match-define (syntax-replacement #:original-syntax orig
                                    #:new-syntax new
                                    #:introduction-scope intro)
    replacement)
  (for/list ([new-id (in-syntax-identifiers new)]
             #:unless (bound-identifier=? new-id (intro new-id 'remove))
             #:unless (free-identifier=? new-id (datum->syntax orig (syntax->datum new-id)))
             #:unless (syntax-property new-id 'skip-incorrect-binding-check?))
    new-id))


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
  (string-replacement-preserved-locations (syntax-replacement-render replacement #:format? #false)))


(define (syntax-source-range stx)
  (define start (sub1 (syntax-position stx)))
  (closed-open-range start (+ start (syntax-span stx)) #:comparator natural<=>))


(define (syntax-completely-original? stx)
  (for/and ([subform (in-stream (syntax-search-everything stx))])
    (syntax-original? subform)))
