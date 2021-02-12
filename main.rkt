#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [refactor! (-> (sequence/c refactoring-result?) void?)]
  [refactor (->* (string?) (#:rules (sequence/c refactoring-rule?)) string-replacement?)]
  [refactor-file (-> path-string? (listof refactoring-result?))]
  [refactor-directory (-> path-string? (listof refactoring-result?))]
  [refactor-package (-> path-string? (listof refactoring-result?))]
  [refactoring-result? predicate/c]
  [refactoring-result
   (-> #:source source-code?
       #:rule-name interned-symbol?
       #:message string?
       #:replacement syntax-replacement?
       refactoring-result?)]
  [refactoring-result-source (-> refactoring-result? source-code?)]
  [refactoring-result-rule-name (-> refactoring-result? interned-symbol?)]
  [refactoring-result-message (-> refactoring-result? immutable-string?)]
  [refactoring-result-replacement (-> refactoring-result? syntax-replacement?)]
  [refactoring-result-original-line (-> refactoring-result? exact-positive-integer?)]
  [refactoring-result-original-code (-> refactoring-result? code-snippet?)]
  [refactoring-result-new-code (-> refactoring-result? code-snippet?)]))


(require fancy-app
         framework
         pkg/lib
         (only-in racket/class
                  new
                  send)
         racket/file
         racket/path
         racket/sequence
         racket/string
         rebellion/base/immutable-string
         rebellion/base/option
         rebellion/base/symbol
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/code-snippet
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         resyntax/default-recommendations
         resyntax/source-code
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
     (substring (source-code-read-string (refactoring-result-source result)) start end)))
  (code-snippet raw-text start-column (syntax-line original)))


(define (refactoring-result-new-code result)
  (define original (syntax-replacement-original-syntax (refactoring-result-replacement result)))
  (define original-line (syntax-line original))
  (define original-column (syntax-column original))
  (define start (sub1 (syntax-position original)))
  (define replacement (syntax-replacement-render (refactoring-result-replacement result)))
  (define end (+ start (string-replacement-new-span replacement)))
  (define source-code (source-code-read-string (refactoring-result-source result)))
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


(define (refactoring-rules-refactor rules syntax source)
  (define (refactor rule)
    (option-map (refactoring-rule-refactor rule syntax)
                (refactoring-result
                 #:source source
                 #:rule-name (object-name rule)
                 #:message (refactoring-rule-description rule)
                 #:replacement _)))
  (falsey->option
   (for*/first ([rule (in-list rules)]
                [result (in-option (refactor rule))])
     result)))


(define (refactor code-string #:rules [rules default-recommendations])
  (define rule-list (sequence->list rules))
  (define source (string-source-code code-string))
  (parameterize ([current-namespace (make-base-namespace)])
    (define analysis (source-code-analyze source))
    (transduce
     (source-code-analysis-visited-forms analysis)
     (append-mapping (λ (stx) (in-option (refactoring-rules-refactor rule-list stx source))))
     (mapping refactoring-result-string-replacement)
     #:into union-into-string-replacement)))


(define (refactor-file path-string #:rules [rules default-recommendations])
  (define path (simple-form-path path-string))
  (printf "resyntax: analyzing ~a\n" path)
  (define rule-list (sequence->list rules))
  (define source (file-source-code path))

  (define (skip e)
    (printf "resyntax: skipping ~a due to syntax error: ~e\n" path (exn-message e))
    empty-list)
  
  (with-handlers ([exn:fail:syntax? skip])
    (parameterize ([current-namespace (make-base-namespace)])
      (define analysis (source-code-analyze source))
      (transduce
       (source-code-analysis-visited-forms analysis)
       (append-mapping
        (λ (stx) (in-option (refactoring-rules-refactor rule-list stx source))))
       #:into into-list))))


(define (refactor-directory path-string #:rules [rules default-recommendations])
  (define path (simple-form-path path-string))
  (define rule-list (sequence->list rules))
  (transduce (in-directory path)
             (filtering rkt-file?)
             (append-mapping (refactor-file _ #:rules rule-list))
             #:into into-list))


(define (refactor-package package-name #:rules [rules default-recommendations])
  (refactor-directory (pkg-directory package-name) #:rules rules))


(define/guard (rkt-file? path)
  (guard (path-has-extension? path #".rkt") else
    #false)
  (define content (file->string path))
  (string-prefix? content "#lang racket/base"))


(define (refactor! results)
  (define results-by-path
    (transduce results
               (bisecting
                (λ (result) (file-source-code-path (refactoring-result-source result)))
                refactoring-result-string-replacement)
               (grouping union-into-string-replacement)
               #:into into-hash))
  (for ([(path replacement) (in-hash results-by-path)])
    (file-apply-string-replacement! path replacement)))
