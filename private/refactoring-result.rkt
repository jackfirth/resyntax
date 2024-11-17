#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactoring-result? (-> any/c boolean?)]
  [refactoring-result
   (-> #:rule-name interned-symbol?
       #:message string?
       #:syntax-replacement syntax-replacement?
       refactoring-result?)]
  [refactoring-result-rule-name (-> refactoring-result? interned-symbol?)]
  [refactoring-result-message (-> refactoring-result? immutable-string?)]
  [refactoring-result-source (-> refactoring-result? source?)]
  [refactoring-result-modified-range (-> refactoring-result? range?)]
  [refactoring-result-modified-line-range (-> refactoring-result? range?)]
  [refactoring-result-syntax-replacement (-> refactoring-result? syntax-replacement?)]
  [refactoring-result-string-replacement (-> refactoring-result? string-replacement?)]
  [refactoring-result-line-replacement (-> refactoring-result? line-replacement?)]
  [refactoring-result-original-line (-> refactoring-result? exact-positive-integer?)]
  [refactoring-result-original-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-new-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-set? (-> any/c boolean?)]
  [refactoring-result-set
   (-> #:base-source source? #:results (sequence/c refactoring-result?) refactoring-result-set?)]
  [refactoring-result-set-base-source (-> refactoring-result-set? source?)]
  [refactoring-result-set-updated-source (-> refactoring-result-set? modified-source?)]
  [refactoring-result-set-results (-> refactoring-result-set? (listof refactoring-result?))]
  [refactoring-result-set-modified-lines (-> refactoring-result-set? immutable-range-set?)]))


(require racket/sequence
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/range
         rebellion/base/symbol
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/code-snippet
         resyntax/private/line-replacement
         resyntax/private/linemap
         resyntax/private/source
         resyntax/private/string-replacement
         resyntax/private/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(define-record-type refactoring-result
  (rule-name message syntax-replacement string-replacement line-replacement)
  #:omit-root-binding)


(define (refactoring-result #:rule-name rule-name #:message message #:syntax-replacement replacement)
  (define str-replacement (syntax-replacement-render replacement))
  (define full-orig-code (source->string (syntax-replacement-source replacement)))
  (constructor:refactoring-result
   #:rule-name rule-name
   #:message (string->immutable-string message)
   #:syntax-replacement replacement
   #:string-replacement str-replacement
   #:line-replacement (string-replacement->line-replacement str-replacement full-orig-code)))


(define (refactoring-result-source result)
  (syntax-replacement-source (refactoring-result-syntax-replacement result)))


(define (refactoring-result-modified-range result)
  (define replacement (refactoring-result-string-replacement result))
  (closed-open-range (add1 (string-replacement-start replacement))
                     (add1 (string-replacement-original-end replacement))
                     #:comparator natural<=>))


(define (refactoring-result-modified-line-range result)
  (define replacement (refactoring-result-line-replacement result))
  (closed-open-range (line-replacement-start-line replacement)
                     (line-replacement-original-end-line replacement)
                     #:comparator natural<=>))


(define (refactoring-result-original-line result)
  (line-replacement-start-line (refactoring-result-line-replacement result)))


(define-record-type refactoring-result-set (base-source results)
  #:omit-root-binding)


(define (refactoring-result-set #:base-source base-source #:results results)
  (define sorted-results
    (transduce results (sorting #:key refactoring-result-original-line) #:into into-list))
  (constructor:refactoring-result-set #:base-source base-source #:results sorted-results))


(define (refactoring-result-set-updated-source result-set)
  (define replacement
    (transduce (refactoring-result-set-results result-set)
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


(define (refactoring-result-original-code result)
  (define replacement (refactoring-result-string-replacement result))
  (define full-orig-code
    (source->string (syntax-replacement-source (refactoring-result-syntax-replacement result))))
  (define lmap (string-linemap full-orig-code))
  (define start (string-replacement-start replacement))
  (define end (string-replacement-original-end replacement))
  (define start-column (- (add1 start) (linemap-position-to-start-of-line lmap (add1 start))))
  (define raw-text (string->immutable-string (substring full-orig-code start end)))
  (code-snippet raw-text start-column (linemap-position-to-line lmap (add1 start))))


(define (refactoring-result-new-code result)
  (define replacement (refactoring-result-string-replacement result))
  (define full-orig-code
    (source->string (syntax-replacement-source (refactoring-result-syntax-replacement result))))
  (define lmap (string-linemap full-orig-code))
  (define start (string-replacement-start replacement))
  (define original-line (linemap-position-to-line lmap (add1 start)))
  (define original-column (- (add1 start) (linemap-position-to-start-of-line lmap (add1 start))))
  (define refactored-source-code (string-apply-replacement full-orig-code replacement))
  (define new-code-string
    (substring refactored-source-code
               (string-replacement-start replacement)
               (string-replacement-new-end replacement)))
  (code-snippet new-code-string original-column original-line))
