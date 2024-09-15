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
  [refactoring-result-syntax-replacement (-> refactoring-result? syntax-replacement?)]
  [refactoring-result-string-replacement (-> refactoring-result? string-replacement?)]
  [refactoring-result-line-replacement (-> refactoring-result? line-replacement?)]
  [refactoring-result-original-line (-> refactoring-result? exact-positive-integer?)]
  [refactoring-result-original-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-new-code (-> refactoring-result? code-snippet?)]))


(require fancy-app
         fmt
         guard
         (only-in racket/list last)
         racket/match
         racket/string
         rebellion/base/immutable-string
         rebellion/base/range
         rebellion/base/symbol
         rebellion/type/record
         resyntax/private/code-snippet
         resyntax/private/line-replacement
         resyntax/private/linemap
         resyntax/private/logger
         resyntax/private/source
         resyntax/private/string-indent
         resyntax/private/string-replacement
         resyntax/private/syntax-range
         resyntax/private/syntax-replacement)


(module+ test
  (require (submod "..")
           rackunit))


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
  (syntax-source-range
   (syntax-replacement-original-syntax (refactoring-result-syntax-replacement result))))


(define (refactoring-result-original-position result)
  (define original
    (syntax-replacement-original-syntax (refactoring-result-syntax-replacement result)))
  (sub1 (syntax-position original)))


(define (refactoring-result-original-line result)
  (syntax-line (syntax-replacement-original-syntax (refactoring-result-syntax-replacement result))))


(define (refactoring-result-original-code result)
  (define stx-replacement (refactoring-result-syntax-replacement result))
  (define original (syntax-replacement-original-syntax stx-replacement))
  (define start (sub1 (syntax-position original)))
  (define end (+ start (syntax-span original)))
  (define start-column (syntax-column original))
  (define raw-text
    (string->immutable-string
     (substring (source->string (syntax-replacement-source stx-replacement)) start end)))
  (code-snippet raw-text start-column (syntax-line original)))


(struct string-slice (full-string subrange-start subrange-end) #:transparent)


(define (string-slice-substring subrange)
  (substring (string-slice-full-string subrange)
             (string-slice-subrange-start subrange)
             (string-slice-subrange-end subrange)))


(define (refactoring-result-new-code result)
  (define stx-replacement (refactoring-result-syntax-replacement result))
  (define original (syntax-replacement-original-syntax stx-replacement))
  (define original-line (syntax-line original))
  (define original-column (syntax-column original))
  (define replacement (refactoring-result-string-replacement result))
  (define source-code (source->string (syntax-replacement-source stx-replacement)))
  (define refactored-source-code (string-apply-replacement source-code replacement))
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
  (line-replacement
   #:start-line
   (syntax-line (syntax-replacement-original-syntax (refactoring-result-syntax-replacement result)))
   #:original-lines (refactoring-result-original-code-lines result)
   #:new-lines (refactoring-result-new-code-lines result)))


(define (refactoring-result-original-code-lines result)
  (define stx-replacement (refactoring-result-syntax-replacement result))
  (define source-code (source->string (syntax-replacement-source stx-replacement)))
  (define map (string-linemap source-code))
  (define original (syntax-replacement-original-syntax stx-replacement))
  (define start (syntax-start-line-position original #:linemap map))
  (define end (syntax-end-line-position original #:linemap map))
  (define original-text (string->immutable-string (substring source-code (sub1 start) (sub1 end))))
  (in-lines (open-input-string original-text)))


(define (refactoring-result-new-code-lines result)
  (define stx-replacement (refactoring-result-syntax-replacement result))
  (define original (syntax-replacement-original-syntax stx-replacement))
  (define source-code (source->string (syntax-replacement-source stx-replacement)))
  (define replacement (refactoring-result-string-replacement result))
  (define start (string-replacement-start replacement))
  (define end (string-replacement-new-end replacement))
  (define refactored-source-code (string-apply-replacement source-code replacement))

  (define map (string-linemap refactored-source-code))
  (define replacement-text
    (string->immutable-string
     (substring refactored-source-code
                (sub1 (linemap-position-to-start-of-line map start))
                (sub1 (linemap-position-to-end-of-line map end)))))
  (in-lines (open-input-string replacement-text)))
