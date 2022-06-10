#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactoring-result? predicate/c]
  [refactoring-result
   (-> #:source source?
       #:rule-name interned-symbol?
       #:message string?
       #:replacement syntax-replacement?
       refactoring-result?)]
  [refactoring-result-source (-> refactoring-result? source?)]
  [refactoring-result-rule-name (-> refactoring-result? interned-symbol?)]
  [refactoring-result-message (-> refactoring-result? immutable-string?)]
  [refactoring-result-replacement (-> refactoring-result? syntax-replacement?)]
  [refactoring-result-string-replacement (-> refactoring-result? string-replacement?)]
  [refactoring-result-line-replacement (-> refactoring-result? line-replacement?)]
  [refactoring-result-original-line (-> refactoring-result? exact-positive-integer?)]
  [refactoring-result-original-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-new-code (-> refactoring-result? code-snippet?)]))


(require fancy-app
         framework
         (only-in racket/class
                  new
                  send)
         racket/string
         rebellion/base/immutable-string
         rebellion/base/symbol
         rebellion/type/record
         resyntax/code-snippet
         resyntax/line-replacement
         resyntax/linemap
         resyntax/source
         resyntax/string-replacement
         resyntax/syntax-replacement)


;@----------------------------------------------------------------------------------------------------


(define-record-type refactoring-result (source rule-name message replacement)
  #:omit-root-binding)


(define (refactoring-result
         #:source source #:rule-name rule-name #:message message #:replacement replacement)
  (constructor:refactoring-result
   #:source source
   #:rule-name rule-name
   #:message (string->immutable-string message)
   #:replacement replacement))


(define (refactoring-result-original-position result)
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (sub1 (syntax-position original)))


(define (refactoring-result-original-line result)
  (syntax-line (syntax-replacement-original-syntax (refactoring-result-replacement result))))


(define (refactoring-result-original-code result)
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (define start (sub1 (syntax-position original)))
  (define end (+ start (syntax-span original)))
  (define start-column (syntax-column original))
  (define raw-text
    (string->immutable-string
     (substring (source->string (refactoring-result-source result)) start end)))
  (code-snippet raw-text start-column (syntax-line original)))


(define (refactoring-result-new-code result)
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (define original-line (syntax-line original))
  (define original-column (syntax-column original))
  (define start (sub1 (syntax-position original)))
  (define replacement (syntax-replacement-render (refactoring-result-replacement result)))
  (define end (+ start (string-replacement-new-span replacement)))
  (define source-code (source->string (refactoring-result-source result)))
  (define refactored-source-code (string-apply-replacement source-code replacement))
  (cond
    [(string-contains? (substring refactored-source-code start end) "\n")
     (define text-object (new racket:text%))
     (send text-object insert refactored-source-code)
     (send text-object set-position start end)
     (send text-object tabify-selection)
     (define indented-start (send text-object get-start-position))
     (define indented-end (send text-object get-end-position))
     (define indented-raw-text
       (string->immutable-string (send text-object get-text indented-start indented-end)))
     (define indented-column (+ original-column (- indented-start start)))
     (code-snippet indented-raw-text indented-column original-line)]
    [else
     (code-snippet (substring refactored-source-code start end) original-column original-line)]))


(define (refactoring-result-string-replacement result)
  (define old-start (refactoring-result-original-position result))
  (define old-code (code-snippet-raw-text (refactoring-result-original-code result)))
  (define new-code (code-snippet-raw-text (refactoring-result-new-code result)))
  (define old-end (+ old-start (string-length old-code)))
  (string-replacement
   #:start old-start
   #:end old-end
   #:contents (list (inserted-string new-code))))


(define (refactoring-result-original-code-lines result)
  (define source-code (source->string (refactoring-result-source result)))
  (define map (string-linemap source-code))
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (define start (syntax-start-line-position original #:linemap map))
  (define end (syntax-end-line-position original #:linemap map))
  (define original-text (string->immutable-string (substring source-code (sub1 start) (sub1 end))))
  (printf "DEBUG: original code lines raw string ~v\n\nDEBUG: linemap below\n\n~a\n\n"
          original-text map)
  (in-lines (open-input-string original-text)))


(define (refactoring-result-new-code-lines result)
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (define start (syntax-position original))
  (define replacement (syntax-replacement-render (refactoring-result-replacement result)))
  (define end (+ start (string-replacement-new-span replacement)))
  (define source-code (source->string (refactoring-result-source result)))
  (define refactored-source-code (string-apply-replacement source-code replacement))
  (define text-object (new racket:text%))
  (send text-object insert refactored-source-code)
  (send text-object set-position (sub1 start) (sub1 end))
  (send text-object tabify-all)
  (define indented-start (send text-object get-start-position))
  (define indented-end (send text-object get-end-position))
  (define all-indented-raw-text (string->immutable-string (send text-object get-text)))
  (define map (string-linemap all-indented-raw-text))
  (define replacement-text
    (string->immutable-string
     (substring all-indented-raw-text
                (linemap-position-to-start-of-line map indented-start)
                (linemap-position-to-end-of-line map indented-end))))
  (in-lines (open-input-string replacement-text)))


(define (refactoring-result-line-replacement result)
  (line-replacement
   #:start-line
   (syntax-line (syntax-replacement-original-syntax (refactoring-result-replacement result)))
   #:original-lines (refactoring-result-original-code-lines result)
   #:new-lines (refactoring-result-new-code-lines result)))
