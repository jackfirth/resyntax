#lang racket/base


(require racket/contract/base)


(provide
 (contract-out
  [source? (-> any/c boolean?)]
  [unmodified-source? (-> any/c boolean?)]
  [source->string (-> source? immutable-string?)]
  [source-path (-> source? (or/c path? #false))]
  [source-directory (-> source? (or/c path? #false))]
  [source-original (-> source? unmodified-source?)]
  [source-read-syntax (-> source? syntax?)]
  [source-analyze (->* (source?) (#:lines range-set?) source-code-analysis?)]
  [file-source? (-> any/c boolean?)]
  [file-source (-> path-string? file-source?)]
  [file-source-path (-> file-source? path?)]
  [string-source? (-> any/c boolean?)]
  [string-source (-> string? string-source?)]
  [string-source-contents (-> string-source? immutable-string?)]
  [modified-source? (-> any/c boolean?)]
  [modified-source (-> unmodified-source? string? modified-source?)]
  [modified-source-contents (-> modified-source? immutable-string?)]
  [modified-source-original (-> modified-source? unmodified-source?)]
  [source-code-analysis? (-> any/c boolean?)]
  [source-code-analysis-code (-> source-code-analysis? source?)]
  [source-code-analysis-visited-forms (-> source-code-analysis? (listof syntax?))]
  [source-code-analysis-expansion-time-output (-> source-code-analysis? immutable-string?)]
  [syntax-source-location (-> syntax? source-location?)]
  [with-input-from-source (-> source? (-> any) any)]))


(require guard
         racket/file
         racket/hash
         racket/match
         racket/path
         racket/port
         rebellion/base/comparator
         rebellion/base/immutable-string
         rebellion/base/range
         rebellion/collection/list
         rebellion/collection/range-set
         rebellion/collection/vector/builder
         rebellion/streaming/reducer
         rebellion/streaming/transducer
         rebellion/type/record
         resyntax/private/fully-expanded-syntax
         resyntax/private/linemap
         resyntax/private/logger
         syntax/id-table
         syntax/modread
         syntax/parse)


;@----------------------------------------------------------------------------------------------------


(struct source () #:transparent)


(struct unmodified-source source () #:transparent)


(struct file-source unmodified-source (path)
  #:transparent
  #:guard (λ (path _) (simple-form-path path)))


(struct string-source unmodified-source (contents)
  #:transparent
  #:guard (λ (contents _) (string->immutable-string contents)))


(struct modified-source source (original contents)
  #:transparent
  #:guard (λ (original contents _) (values original (string->immutable-string contents))))


(define-record-type source-code-analysis (code visited-forms expansion-time-output))
(define-record-type source-location (source line column position span))


(define (with-input-from-source code proc)

  (define (call-proc-with-reencoded-input in)
    (define reencoded-in (reencode-input-port in "UTF-8" #false #false (object-name in) #true))
    (parameterize ([current-input-port reencoded-in])
      (proc)))

  (match code
    [(file-source path) (call-with-input-file path call-proc-with-reencoded-input)]
    [(string-source contents) (call-proc-with-reencoded-input (open-input-string contents))]
    [(modified-source (file-source path) contents)
     (call-proc-with-reencoded-input (open-input-string contents path))]
    [(modified-source (? string-source?) contents)
     (call-proc-with-reencoded-input (open-input-string contents))]))


(define (source->string code)
  (string->immutable-string (with-input-from-source code port->string)))


(define (source-read-syntax code)
  (define (read-from-input)
    (port-count-lines! (current-input-port))
    (with-module-reading-parameterization read-syntax))
  (with-input-from-source code read-from-input))


(define/guard (source-path code)
  (guard-match (or (file-source path) (modified-source (file-source path) _)) code #:else #false)
  path)


(define/guard (source-directory code)
  (define path (source-path code))
  (and path (path-only path)))


(define (source-original code)
  (if (unmodified-source? code)
      code
      (modified-source-original code)))


(define (source-analyze code #:lines [lines (range-set (unbounded-range #:comparator natural<=>))])
  (parameterize ([current-directory (or (source-directory code) (current-directory))])
    (define code-linemap (string-linemap (source->string code)))
    (define program-stx (source-read-syntax code))
    (define program-source-name (syntax-source program-stx))
    (define current-expand-observe (dynamic-require ''#%expobs 'current-expand-observe))
    (define original-visits (make-vector-builder))
    (define expanded-originals-by-location (make-hash))

    (define (add-all-original-subforms! stx)
      (when (resyntax-should-analyze-syntax? stx #:as-visit? #false)
        (hash-set! expanded-originals-by-location (syntax-source-location stx) stx))
      (syntax-parse stx
        [(subform ...) (for-each add-all-original-subforms! (attribute subform))]
        [(subform ...+ . tail-form)
         (for-each add-all-original-subforms! (attribute subform))
         (add-all-original-subforms! #'tail-form)]
        [_ (void)]))

    (define (syntax-original-and-from-source? stx)
      (and (syntax-original? stx)
           ;; Some macros are able to bend hygiene and syntax properties in such a way that they
           ;; introduce syntax objects into the program that are syntax-original?, but from a
           ;; different file than the one being expanded. So in addition to checking for
           ;; originality, we also check that they come from the same source as the main program
           ;; syntax object. The (open ...) clause of the define-signature macro bends hygiene
           ;; in this way, and is what originally motivated the addition of this check.
           (equal? (syntax-source stx) program-source-name)))

    (define/guard (resyntax-should-analyze-syntax? stx #:as-visit? [as-visit? #true])
      (guard (syntax-original-and-from-source? stx) #:else #false)
      (guard as-visit? #:else #true)
      (define stx-lines (syntax-line-range stx #:linemap code-linemap))
      (define overlaps? (range-set-overlaps? lines stx-lines))
      (unless overlaps?
        (log-resyntax-debug
         (string-append "ignoring visited syntax object because it's outside analyzed lines\n"
                        "  analyzed lines: ~a\n"
                        "  syntax lines: ~a\n"
                        "  syntax: ~a")
         lines
         stx-lines
         stx))
      overlaps?)
    
    (define/match (observe-event! sig val)
      [('visit (? syntax? visited))
       (when (resyntax-should-analyze-syntax? visited)
         (vector-builder-add original-visits visited)
         (add-all-original-subforms! visited))]
      [(_ _) (void)])

    (define output-port (open-output-string))
    (define expanded
      (parameterize ([current-expand-observe observe-event!]
                     [current-output-port output-port])
        (expand program-stx)))

    (define output (get-output-string output-port))
    (define binding-table (fully-expanded-syntax-binding-table expanded))
    (define original-binding-table-by-position
      (for*/fold ([table (hash)])
                 ([phase-table (in-hash-values binding-table)]
                  [(id uses) (in-free-id-table phase-table)]
                  #:when (syntax-original-and-from-source? id)
                  [use (in-list uses)])
        (hash-update table (syntax-source-location id) (λ (previous) (cons use previous)) '())))
    
    (add-all-original-subforms! expanded)

    (define/guard (add-usages stx)
      (guard (identifier? stx) #:else stx)
      (define usages (hash-ref original-binding-table-by-position (syntax-source-location stx) '()))
      (syntax-property stx 'identifier-usages usages))

    (define (enrich stx)
      (define new-context
        (add-usages
         (or (hash-ref expanded-originals-by-location (syntax-source-location stx) #false) stx)))
      (syntax-parse stx
        [(subform ...)
         (datum->syntax new-context
                        (map enrich (attribute subform))
                        new-context
                        new-context)]
        [(subform ...+ . tail-form)
         (datum->syntax new-context
                        (append (map enrich (attribute subform)) (enrich #'tail-form))
                        new-context
                        new-context)]
        [_ new-context]))
         
    
    (define visited
      (transduce (build-vector original-visits)
                 (deduplicating #:key syntax-source-location)
                 (mapping enrich)
                 (sorting syntax-source-location<=> #:key syntax-source-location)
                 #:into into-list))

    (source-code-analysis #:code code #:visited-forms visited #:expansion-time-output output)))


(define (syntax-source-location stx)
  (source-location
   #:source (syntax-source stx)
   #:line (syntax-line stx)
   #:column (syntax-column stx)
   #:position (syntax-position stx)
   #:span (syntax-span stx)))


(define syntax-source-location<=>
  (comparator-chain (comparator-map real<=> source-location-position)
                    (comparator-map (comparator-reverse real<=>) source-location-span)))
