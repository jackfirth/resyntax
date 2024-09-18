#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactoring-result? predicate/c]
  [refactoring-result
   (-> #:rule-name interned-symbol?
       #:message string?
       #:syntax-replacement syntax-replacement?
       refactoring-result?)]
  [refactoring-result-rule-name (-> refactoring-result? interned-symbol?)]
  [refactoring-result-message (-> refactoring-result? immutable-string?)]
  [refactoring-result-modified-range (-> refactoring-result? range?)]
  [refactoring-result-modified-line-range (-> refactoring-result? range?)]
  [refactoring-result-syntax-replacement (-> refactoring-result? syntax-replacement?)]
  [refactoring-result-string-replacement (-> refactoring-result? string-replacement?)]
  [refactoring-result-line-replacement (-> refactoring-result? line-replacement?)]
  [refactoring-result-original-line (-> refactoring-result? exact-positive-integer?)]
  [refactoring-result-original-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-new-code (-> refactoring-result? code-snippet?)]))


(require rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/range
         rebellion/base/symbol
         rebellion/type/record
         resyntax/private/code-snippet
         resyntax/private/line-replacement
         resyntax/private/linemap
         resyntax/private/source
         resyntax/private/string-replacement
         resyntax/private/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(define-record-type refactoring-result (rule-name message syntax-replacement string-replacement)
  #:omit-root-binding)


(define (refactoring-result #:rule-name rule-name #:message message #:syntax-replacement replacement)
  (constructor:refactoring-result
   #:rule-name rule-name
   #:message (string->immutable-string message)
   #:syntax-replacement replacement
   #:string-replacement (syntax-replacement-render replacement)))


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


;; Like refactoring-result-string-replacement, but for generating a source code replacement across a
;; range of lines in the source code text rather than exact positions. This is used by Resyntax when
;; it is necessary to avoid conflicts between technically independent refactoring results whose lines
;; overlap, which can cause problems in various UIs. GitHub pull request comments, for instance, are
;; not pleasant to read when there are multiple independent comment threads on the same line of code.
;; When Resyntax actually fixes files directly via the `resyntax fix` command, the more precise
;; refactoring-result-string-replacement function is used instead because the results do not have to
;; be displayed in a UI.
(define (refactoring-result-line-replacement result)
  (define full-orig-code
    (source->string (syntax-replacement-source (refactoring-result-syntax-replacement result))))
  (string-replacement->line-replacement (refactoring-result-string-replacement result)
                                        full-orig-code))
