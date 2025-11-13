#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactoring-result? (-> any/c boolean?)]
  [refactoring-result
   (->* (#:rule-name interned-symbol?
         #:message string?)
        (#:syntax-replacement (or/c syntax-replacement? #false))
        refactoring-result?)]
  [warning-result
   (-> #:rule-name interned-symbol?
       #:message string?
       #:source source?
       #:original-syntax syntax?
       refactoring-result?)]
  [refactoring-result-rule-name (-> refactoring-result? interned-symbol?)]
  [refactoring-result-message (-> refactoring-result? immutable-string?)]
  [refactoring-result-source (-> refactoring-result? source?)]
  [refactoring-result-has-fix? (-> refactoring-result? boolean?)]
  [refactoring-result-modified-range (-> refactoring-result? range?)]
  [refactoring-result-modified-line-range (-> refactoring-result? range?)]
  [refactoring-result-syntax-replacement (-> refactoring-result? (or/c syntax-replacement? #false))]
  [refactoring-result-string-replacement (-> refactoring-result? (or/c string-replacement? #false))]
  [refactoring-result-line-replacement (-> refactoring-result? (or/c line-replacement? #false))]
  [refactoring-result-original-line (-> refactoring-result? exact-positive-integer?)]
  [refactoring-result-original-column (-> refactoring-result? exact-nonnegative-integer?)]
  [refactoring-result-original-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-new-code (-> refactoring-result? (or/c code-snippet? #false))]
  [refactoring-result-set? (-> any/c boolean?)]
  [refactoring-result-set
   (-> #:base-source source? #:results (sequence/c refactoring-result?) refactoring-result-set?)]
  [refactoring-result-set-base-source (-> refactoring-result-set? source?)]
  [refactoring-result-set-updated-source (-> refactoring-result-set? modified-source?)]
  [refactoring-result-set-results (-> refactoring-result-set? (listof refactoring-result?))]
  [refactoring-result-set-modified-lines (-> refactoring-result-set? immutable-range-set?)]
  [refactoring-result-set-compiles? (-> refactoring-result-set? boolean?)]
  [refactoring-result-map-commits
   (-> (hash/c source? refactoring-result-set?) (listof resyntax-commit?))]))


(require racket/hash
         racket/sequence
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/range
         rebellion/base/symbol
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/collection/sorted-set
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/code-snippet
         resyntax/private/commit
         resyntax/private/line-replacement
         resyntax/private/linemap
         resyntax/private/logger
         resyntax/private/source
         resyntax/private/string-replacement
         resyntax/private/syntax-replacement
         (only-in racket/list first))


;@----------------------------------------------------------------------------------------------------


(define-record-type refactoring-result
  (rule-name message source original-syntax syntax-replacement string-replacement line-replacement)
  #:omit-root-binding)


(define (refactoring-result #:rule-name rule-name 
                            #:message message 
                            #:syntax-replacement [replacement #false])
  (if replacement
      (let ([str-replacement (syntax-replacement-render replacement)]
            [full-orig-code (source->string (syntax-replacement-source replacement))])
        (constructor:refactoring-result
         #:rule-name rule-name
         #:message (string->immutable-string message)
         #:source (syntax-replacement-source replacement)
         #:original-syntax (syntax-replacement-original-syntax replacement)
         #:syntax-replacement replacement
         #:string-replacement str-replacement
         #:line-replacement (string-replacement->line-replacement str-replacement full-orig-code)))
      (raise-arguments-error 'refactoring-result
                            "must provide either #:syntax-replacement"
                            "rule-name" rule-name
                            "message" message)))


(define (warning-result #:rule-name rule-name #:message message #:source source #:original-syntax original-syntax)
  (constructor:refactoring-result
   #:rule-name rule-name
   #:message (string->immutable-string message)
   #:source source
   #:original-syntax original-syntax
   #:syntax-replacement #false
   #:string-replacement #false
   #:line-replacement #false))


(define (refactoring-result-has-fix? result)
  (and (refactoring-result-syntax-replacement result) #true))


(define (refactoring-result-modified-range result)
  (cond
    [(refactoring-result-has-fix? result)
     (define replacement (refactoring-result-string-replacement result))
     (closed-open-range (add1 (string-replacement-start replacement))
                        (add1 (string-replacement-original-end replacement))
                        #:comparator natural<=>)]
    [else
     (define orig-stx (refactoring-result-original-syntax result))
     (define pos (syntax-position orig-stx))
     (define span (syntax-span orig-stx))
     (if (and pos span)
         (closed-open-range pos (+ pos span) #:comparator natural<=>)
         (closed-open-range 1 2 #:comparator natural<=>))]))


(define (refactoring-result-modified-line-range result)
  (cond
    [(refactoring-result-has-fix? result)
     (define replacement (refactoring-result-line-replacement result))
     (closed-open-range (line-replacement-start-line replacement)
                        (line-replacement-original-end-line replacement)
                        #:comparator natural<=>)]
    [else
     (define orig-stx (refactoring-result-original-syntax result))
     (define line (syntax-line orig-stx))
     (if line
         (closed-range line line #:comparator natural<=>)
         (closed-range 1 1 #:comparator natural<=>))]))


(define (refactoring-result-original-line result)
  (if (refactoring-result-has-fix? result)
      (line-replacement-start-line (refactoring-result-line-replacement result))
      (or (syntax-line (refactoring-result-original-syntax result)) 1)))


(define (refactoring-result-original-column result)
  (if (refactoring-result-has-fix? result)
      (code-snippet-start-column (refactoring-result-original-code result))
      (or (syntax-column (refactoring-result-original-syntax result)) 0)))


(define-record-type refactoring-result-set (base-source results)
  #:omit-root-binding)


(define (refactoring-result-set #:base-source base-source #:results results)
  (define sorted-results
    (transduce results (sorting #:key refactoring-result-original-line) #:into into-list))
  (constructor:refactoring-result-set #:base-source base-source #:results sorted-results))


(define (refactoring-result-set-updated-source result-set)
  (define replacement
    (transduce (refactoring-result-set-results result-set)
               (filtering refactoring-result-has-fix?)
               (mapping refactoring-result-string-replacement)
               #:into union-into-string-replacement))
  (define base (refactoring-result-set-base-source result-set))
  (define new-contents (string-apply-replacement (source->string base) replacement))
  (modified-source (source-original base) new-contents))


(define (refactoring-result-set-modified-lines result-set)
  (transduce (refactoring-result-set-results result-set)
             (mapping refactoring-result-modified-line-range)
             (filtering nonempty-range?)
             #:into (into-range-set natural<=>)))


(define (refactoring-result-set-compiles? result-set)
  (source-can-expand? (refactoring-result-set-updated-source result-set)))


(define string-replacement<=> (comparator-map natural<=> string-replacement-start))


(define (refactoring-result-map-commits result-map)
  (define rule-names
    (transduce (in-hash-values result-map)
               (append-mapping refactoring-result-set-results)
               (filtering refactoring-result-has-fix?) ; Only include results with fixes
               (mapping refactoring-result-rule-name)
               (deduplicating)
               #:into into-list))
  (define source-contents
    (for/hash ([source (in-hash-keys result-map)])
      (values source (source->string source))))
  (for/fold ([committed-replacements (hash)]
             [commits '()]
             #:result (reverse commits))
            ([rule (in-list rule-names)])
    (define rule-results
      (for*/list ([results (in-hash-values result-map)]
                  [result (in-list (refactoring-result-set-results results))]
                  #:when (and (equal? (refactoring-result-rule-name result) rule)
                             (refactoring-result-has-fix? result)))
        result))
    (define replacements
      (for/hash ([(source results) (in-hash result-map)])
        (define source-replacements
          (transduce (refactoring-result-set-results results)
                     (filtering (λ (r) (and (equal? (refactoring-result-rule-name r) rule)
                                           (refactoring-result-has-fix? r))))
                     (mapping refactoring-result-string-replacement)
                     #:into (into-sorted-set string-replacement<=>)))
        (values source source-replacements)))
    (define new-committed-replacements
      (hash-union committed-replacements replacements #:combine sorted-set-add-all))
    (define new-contents
      (for/hash ([(source old-contents) (in-hash source-contents)])
        (define replacement
          (transduce (hash-ref new-committed-replacements source '())
                     #:into union-into-string-replacement))
        (values (source-path source) (string-apply-replacement old-contents replacement))))
    (define description
      (refactoring-result-message (first rule-results)))
    (define num-fixes (length rule-results))
    (define message
      (format "Fix ~a occurrence~a of `~a`\n\n~a"
              num-fixes
              (if (equal? num-fixes 1) "" "s")
              rule
              description))
    (define commit (resyntax-commit message new-contents))
    (values new-committed-replacements (cons commit commits))))


(define (refactoring-result-original-code result)
  (cond
    [(refactoring-result-has-fix? result)
     (define replacement (refactoring-result-string-replacement result))
     (define full-orig-code
       (source->string (syntax-replacement-source (refactoring-result-syntax-replacement result))))
     (define lmap (string-linemap full-orig-code))
     (define start (string-replacement-start replacement))
     (define end (string-replacement-original-end replacement))
     (define start-column (- (add1 start) (linemap-position-to-start-of-line lmap (add1 start))))
     (define raw-text (string->immutable-string (substring full-orig-code start end)))
     (code-snippet raw-text start-column (linemap-position-to-line lmap (add1 start)))]
    [else
     (define orig-stx (refactoring-result-original-syntax result))
     (define source (refactoring-result-source result))
     (define full-orig-code (source->string source))
     (define pos (syntax-position orig-stx))
     (define span (syntax-span orig-stx))
     (define line (or (syntax-line orig-stx) 1))
     (define col (or (syntax-column orig-stx) 0))
     (if (and pos span)
         (let* ([start (sub1 pos)]
                [end (+ start span)]
                [raw-text (string->immutable-string (substring full-orig-code start end))])
           (code-snippet raw-text col line))
         (code-snippet "" col line))]))


(define (refactoring-result-new-code result)
  (and (refactoring-result-has-fix? result)
       (let* ([replacement (refactoring-result-string-replacement result)]
              [full-orig-code (source->string (syntax-replacement-source
                                               (refactoring-result-syntax-replacement result)))]
              [lmap (string-linemap full-orig-code)]
              [start (string-replacement-start replacement)]
              [original-line (linemap-position-to-line lmap (add1 start))]
              [original-column (- (add1 start) (linemap-position-to-start-of-line lmap (add1 start)))]
              [refactored-source-code (string-apply-replacement full-orig-code replacement)]
              [new-code-string (substring refactored-source-code
                                          (string-replacement-start replacement)
                                          (string-replacement-new-end replacement))])
         (code-snippet new-code-string original-column original-line))))
