#lang racket/base

(require (for-syntax racket/base)
         (for-template racket/block)
         fancy-app
         racket/block
         racket/file
         racket/format
         racket/function
         racket/match
         racket/port
         racket/pretty
         racket/sequence
         racket/set
         racket/string
         racket/syntax
         rebellion/base/option
         rebellion/collection/entry
         rebellion/collection/hash
         rebellion/collection/list
         rebellion/module/binding
         rebellion/module/phase
         rebellion/private/guarded-block
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         rebellion/type/tuple
         resyntax/refactoring-rule
         (submod resyntax/refactoring-rule private)
         syntax/modread
         syntax/parse
         syntax/parse/define
         syntax/parse/lib/function-header)

(module+ test
  (require (submod "..")
           rackunit))

(module+ main
  (require racket/runtime-path))

;@----------------------------------------------------------------------------------------------------

(define (read-module) (with-module-reading-parameterization read-syntax))

(define (read-module-from-string string)
  (with-input-from-string string
    (λ ()
      (port-count-lines! (current-input-port))
      (read-module))))

(define (refactoring-rule-apply rule module-code-string)
  (parameterize ([current-namespace (make-base-namespace)])
    (define module-syntax (read-module-from-string module-code-string))
    (define events (expansion-events module-syntax))
    (transduce events
               (bisecting expansion-event-sig expansion-event-val)
               (filtering-keys (equal? _ 'visit))
               (mapping entry-value)
               (filtering syntax?)
               (filtering syntax-original?)
               (deduplicating #:key syntax-source-location)
               (bisecting values (refactoring-rule-refactor rule _))
               (append-mapping-values in-option)
               (mapping-values syntax->string)
               (mapping
                (λ (e)
                  (match-define (entry stx code) e)
                  (define pos (syntax-position stx))
                  (define span (syntax-span stx))
                  (source-replacement
                   #:position pos
                   #:span span
                   #:old-code (substring module-code-string (sub1 pos) (+ (sub1 pos) span))
                   #:new-code code)))
               (sorting #:key source-replacement-position)
               #:into into-list)))

(define-tuple-type expansion-event (sig val))
(define-record-type source-location (source line column position span))
(define-record-type source-replacement (position span old-code new-code))

(define (syntax-source-location stx)
  (source-location
   #:source (syntax-source stx)
   #:line (syntax-line stx)
   #:column (syntax-column stx)
   #:position (syntax-position stx)
   #:span (syntax-span stx)))

(define (expansion-events stx)
  (define current-expand-observe (dynamic-require ''#%expobs 'current-expand-observe))
  (define events '())
  (define (add-event! sig val)
    (set! events (cons (expansion-event sig val) events)))
  (parameterize ([current-expand-observe add-event!])
    (expand stx))
  (reverse events))

(define (syntax->string stx)
  (syntax-parse stx
    #:literals (quote)
    [id:id (symbol->string (syntax-e #'id))]
    [(~or v:boolean v:char v:keyword v:number v:regexp v:byte-regexp v:string v:bytes)
     (~v (syntax-e #'v))]
    [(quote datum) (string-append "'" (syntax->string #'datum))]
    [(subform ...)
     (string-join (for/list ([subform-stx (in-syntax #'(subform ...))]) (syntax->string subform-stx))
                  #:before-first "("
                  #:after-last ")")]))

(define (refactor-once code-string rules)
  (apply-replacements code-string (refactor-replacements code-string rules)))

(define (refactor-replacements code-string rules)
  (define replacements
    (transduce rules
               (append-mapping (refactoring-rule-apply _ code-string))
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

(define standard-refactoring-rules
    (list let-to-block single-block-elimination immediate-define-block-elimination))

(define/guard (refactor code-string #:rules [rules standard-refactoring-rules])
  (define replacements (refactor-replacements code-string rules))
  (guard (empty-list? replacements) then code-string)
  (refactor (apply-replacements code-string replacements) #:rules rules))

(define (refactor-file path #:rules [rules standard-refactoring-rules])
  (refactor-replacements (file->string path #:mode 'text) rules))

(define (refactor-file! path #:rules [rules standard-refactoring-rules])
  (define replacement-code (refactor (file->string path #:mode 'text) #:rules rules))
  (display-to-file replacement-code path #:mode 'text #:exists 'replace))

(define (multiline-string . lines)
  (string-join lines "\n" #:after-last "\n"))

(module+ main
  
  (define-runtime-path example1 "examples/example1.rkt")

  (refactor
   "#lang racket/base (require racket/block) (define (add) (let ([a 1] [b 2]) (+ a b)))")

  (refactor-file! example1))
