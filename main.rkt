#lang racket/base

(require (for-syntax racket/base)
         fancy-app
         racket/file
         racket/format
         racket/match
         racket/path
         racket/sequence
         racket/string
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/list
         rebellion/private/guarded-block
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         resyntax/source-code
         resyntax/syntax-rendering
         syntax/parse)

(module+ test
  (require (submod "..")))

;@----------------------------------------------------------------------------------------------------

(define (refactoring-rule-apply rule code)
  (parameterize ([current-namespace (make-base-namespace)])
    (define code-string (source-code-read-string code))
    (define analysis (source-code-analyze code))
    (transduce (source-code-analysis-visited-forms analysis)
               (bisecting values (refactoring-rule-refactor rule _))
               (append-mapping-values in-option)
               (mapping-values (syntax-render _ code code-string))
               (mapping
                (λ (e)
                  (match-define (entry stx code) e)
                  (define pos (syntax-position stx))
                  (define span (syntax-span stx))
                  (source-replacement
                   #:position pos
                   #:span span
                   #:old-code (substring code-string (sub1 pos) (+ (sub1 pos) span))
                   #:new-code code)))
               (sorting #:key source-replacement-position)
               #:into into-list)))

(define-record-type source-replacement (position span old-code new-code))

(define (refactor-source-code code rules)
  (define replacements
    (transduce rules
               (append-mapping (refactoring-rule-apply _ code))
               (sorting #:key source-replacement-position)
               #:into into-list))
  (define/guard (loop [replacements replacements])
    (guard-match (list first second remaining ...) replacements else
      replacements)
    (define first-start (source-replacement-position first))
    (define first-end (+ first-start (source-replacement-span first)))
    (define second-start (source-replacement-position second))
    (define second-end (+ second-start (source-replacement-span second)))
    (guard (< first-end second-end) else
      (loop (cons first remaining)))
    (guard (<= first-end second-start) else
      (error 'overlap))
    (cons first (loop (cons second remaining))))
  (loop))

(define (string-apply-replacement code-string replacement)
  (match-define (source-replacement #:new-code new #:position pos #:span span) replacement)
  (define new-length (- (+ (string-length code-string) (string-length new)) span))
  (define modified-code (make-string new-length))
  (define modification-start (sub1 pos))
  (define modification-end (+ pos (string-length new) -1))
  (string-copy! modified-code 0 code-string 0 modification-start)
  (string-copy! modified-code modification-start new)
  (string-copy! modified-code modification-end code-string (+ pos span -1))
  (string->immutable-string modified-code))

(define (apply-replacements code-string replacements)
  (define descending-replacements
    (transduce replacements
               (sorting #:key source-replacement-position #:descending? #true)
               #:into into-list))
  (for/fold ([code-string code-string]) ([replacement descending-replacements])
    (string-apply-replacement code-string replacement)))

(define/guard (refactor code #:rules [rules standard-refactoring-rules])
  (define code-string (source-code-read-string code))
  (define replacements (refactor-source-code code rules))
  (apply-replacements code-string replacements))

(define (refactor-file path #:rules [rules standard-refactoring-rules])
  (refactor-source-code (file-source-code path) rules))

(define (refactor-file! path #:rules [rules standard-refactoring-rules])
  (define replacement-code (refactor (file-source-code path) #:rules rules))
  (display-to-file replacement-code path #:mode 'text #:exists 'replace))

(define (rkt-path? path) (path-has-extension? path #".rkt"))

(define (refactor-directory! path #:rules [rules standard-refactoring-rules])
  (for ([file (in-directory path)] #:when (rkt-path? file))
    (refactor-file! file #:rules rules)))


(module+ main
  (refactor-directory! "/Users/jackfirth/Documents/GitHub/rackunit/rackunit-lib/rackunit"))
